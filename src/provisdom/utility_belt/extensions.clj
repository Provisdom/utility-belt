(ns provisdom.utility-belt.extensions
  "Extended functionality for Clojure's core data operations.
   Provides macros for conditional binding and functions for working with
   collections, including nested updates, interleaving, and reducing with indices."
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::nilable-seq (s/nilable (s/every any?)))

;;;MACROS
(defmacro if-all-let
  "A variant of if-let that tests multiple bindings, requiring all to be truthy.
  
   Similar to if-let but requires all bindings to be non-nil/non-false.
   If any binding evaluates to nil or false, the else expression is evaluated.
   
   Parameters:
   - bindings: A vector of binding forms (as in let)
   - then: Expression to evaluate if all bindings are truthy
   - else: Expression to evaluate if any binding is falsey (optional, defaults to nil)
   
   Example:
   ```clojure
   (if-all-let [a (get-a)
                b (get-b)
                c (calculate-c a b)]
     (do-something-with a b c)
     (handle-missing-values))
   ```"
  ([bindings then else]
   (reduce (fn [subform binding]
             `(if-let [~@binding] ~subform ~else))
     then
     (reverse (partition 2 bindings))))
  ([bindings then]
   `(if-all-let ~bindings ~then nil)))

(defmacro when-all-let
  "A variant of when-let that tests multiple bindings, requiring all to be truthy.
  
   Similar to when-let but requires all bindings to be non-nil/non-false.
   If any binding evaluates to nil or false, returns nil.
   A simplified version of if-all-let without the else clause.
   
   Parameters:
   - bindings: A vector of binding forms (as in let)
   - then: Expression to evaluate if all bindings are truthy
   
   Example:
   ```clojure
   (when-all-let [a (get-a)
                  b (get-b)
                  c (calculate-c a b)]
     (do-something-with a b c))
   ```"
  [bindings then]
  `(if-all-let ~bindings ~then nil))

;;;FUNCTIONS
(defn update-in-with-not-found
  "Enhanced version of update-in that allows a default value for missing keys.
   
   Like Clojure's update-in, but takes a not-found value that will be passed to
   the function when the key path doesn't exist, instead of nil. Works with
   both maps and vectors.
   
   Parameters:
   - m: The nested associative structure to update
   - [k & ks]: A sequence of keys representing a path into the nested structure
   - f: A function to apply to the value at the specified key path
   - not-found: The value to use if the key path doesn't exist
   - args: Additional arguments to pass to f
   
   Returns:
   - A new nested structure with the value at the key path updated
   - Returns m unchanged if the key path can't be created (e.g., parent path doesn't exist)
   
   Example:
   ```clojure
   (update-in-with-not-found {:a {:b 1}} [:a :c] + 0 10)
   ;; => {:a {:b 1, :c 10}}
   
   (update-in-with-not-found {:a {:b 1}} [:a :b] + 0 10)
   ;; => {:a {:b 11}}
   
   ;; Safely handles non-existent paths in nested structures
   (update-in-with-not-found {} [:a :b :c] conj 0 :x)
   ;; => {}
   ```"
  [m [k & ks] f not-found & args]
  (if (empty? m)
    m
    (if ks
      (let [level1 (get m k not-found)
            next-k (first ks)]
        (when (and (or (map? level1) (and (vector? level1)
                                       (int? next-k)
                                       (>= next-k 0)
                                       (< next-k (count level1))))
                (or (map? m) (and (vector? m)
                               (int? k)
                               (>= k 0)
                               (< k (count m)))))
          (assoc m
            k
            (apply update-in-with-not-found level1 ks f not-found args))))
      (when (or (map? m) (and (vector? m)
                           (int? k)
                           (>= k 0)
                           (< k (count m))))
        (assoc m k (apply f (get m k not-found) args))))))

(s/fdef update-in-with-not-found
  :args (s/cat :m (s/or :map map?
                    :v vector?)
          :ks (s/coll-of any?)
          :f (s/fspec :args (s/cat :v any?
                              :args (s/* any?))
               :ret any?)
          :not-found any?
          :args (s/* any?))
  :ret any?)

(defn interleave-all
  "Enhanced version of interleave that consumes all elements from all collections.
   
   Unlike clojure.core/interleave, which stops at the end of the shortest collection,
   this function continues interleaving by consuming all remaining elements from
   any non-empty collections.
   
   Parameters:
   - colls: Collections to interleave
   
   Returns:
   - A lazy sequence containing all elements from all collections, interleaved
   
   Examples:
   ```clojure
   ;; Basic usage
   (interleave-all [1 2 3] [:a :b])
   ;; => (1 :a 2 :b 3)
   
   ;; With core/interleave, result would be (1 :a 2 :b)
   
   ;; Multiple collections
   (interleave-all [1 2] [:a :b :c] [:A :B])
   ;; => (1 :a :A 2 :b :B :c)
   ```"
  ([] '())
  ([c1]
   (lazy-seq (if (sequential? c1)
               (seq c1)
               '())))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1)
           s2 (seq c2)]
       (cond (and s1 s2)
         (cons (first s1)
           (cons (first s2)
             (interleave-all (rest s1) (rest s2))))
         s1 s1
         s2 s2))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (map seq (conj colls c2 c1))]
       (if (every? identity ss)
         (concat (map first ss)
           (apply interleave-all
             (map rest ss)))
         (let [ns (filter identity ss)]
           (concat (map first ns)
             (apply interleave-all
               (map rest ns)))))))))

(s/fdef interleave-all
  :args (s/cat :c1 (s/? ::nilable-seq)
          :c2 (s/? ::nilable-seq)
          :colls (s/* ::nilable-seq))
  :ret (s/every any?))

(defn reduce-kv-ext
  "Extended version of reduce-kv that works with multiple collections simultaneously.
   
   Like reduce-kv but can process multiple collections in parallel, passing
   corresponding elements from each collection to the reducing function.
   Uses arrays internally for performance.
   
   Parameters:
   - f: A reducing function that takes:
       - For one collection: (result, index, item)
       - For two collections: (result, index, item1, item2)
       - For three collections: (result, index, item1, item2, item3)
   - init: The initial value for the reduction
   - collections: One to three collections to process
   
   Important: The first collection must be the shortest of all provided collections.
   
   Examples:
   ```clojure
   ;; Single collection works like reduce-kv
   (reduce-kv-ext (fn [result idx val] (assoc result idx (* val 2)))
                 {} [1 2 3])
   ;; => {0 2, 1 4, 2 6}
   
   ;; Multiple collections
   (reduce-kv-ext (fn [result idx v1 v2] (conj result [v1 v2]))
                 [] [:a :b] [1 2 3])
   ;; => [[:a 1] [:b 2]]
   ```"
  ([f init coll] (reduce-kv f init coll))
  ([f init c1 c2]
   (let [a1 (to-array c1)
         a2 (to-array c2)]
     (areduce a1 i ret init (f ret i (aget a1 i) (aget a2 i)))))
  ([f init c1 c2 c3]
   (let [a1 (to-array c1)
         a2 (to-array c2)
         a3 (to-array c3)]
     (areduce a1 i ret init (f ret i (aget a1 i) (aget a2 i) (aget a3 i))))))

(s/fdef reduce-kv-ext
  :args (s/or :three (s/cat :f (s/fspec :args (s/cat :result any?
                                                :index int?
                                                :value any?)
                                 :ret any?)
                       :init any?
                       :coll ::nilable-seq)
          :four (s/and (s/cat :f (s/fspec :args (s/cat :result any?
                                                  :index int?
                                                  :value1 any?
                                                  :value2 any?)
                                   :ret any?)
                         :init any?
                         :c1 ::nilable-seq
                         :c2 ::nilable-seq)
                  (fn [{:keys [c1 c2]}]
                    (<= (count c1) (count c2))))
          :five (s/and (s/cat :f (s/fspec :args (s/cat :result any?
                                                  :index int?
                                                  :value1 any?
                                                  :value2 any?
                                                  :value3 any?)
                                   :ret any?)
                         :init any?
                         :c1 ::nilable-seq
                         :c2 ::nilable-seq
                         :c3 ::nilable-seq)
                  (fn [{:keys [c1 c2 c3]}]
                    (and (<= (count c1) (count c2))
                      (<= (count c1) (count c3))))))
  :ret any?)

(defn reductions-kv
  "Returns a lazy seq of a reduction with indices. Function `f` takes the
  result value, an index, and the item value(s)."
  ([f init coll]
   (when-all-let [[init-h & init-t] (seq coll)]
     (letfn [(g [i res coll]
               (cons res
                 (lazy-seq
                   (when-let [[h & t] (seq coll)]
                     (g (inc i) (f res i h) t)))))]
       (g 1 (f init 0 init-h) init-t))))
  ([f init c1 c2]
   (when-all-let [[init-h1 & init-t1] (seq c1)
                  [init-h2 & init-t2] (seq c2)]
     (letfn [(g [i res c1 c2]
               (cons res
                 (lazy-seq
                   (when-all-let [[h1 & t1] (seq c1)
                                  [h2 & t2] (seq c2)]
                     (g (inc i) (f res i h1 h2) t1 t2)))))]
       (g 1 (f init 0 init-h1 init-h2) init-t1 init-t2))))
  ([f init c1 c2 c3]
   (when-all-let [[init-h1 & init-t1] (seq c1)
                  [init-h2 & init-t2] (seq c2)
                  [init-h3 & init-t3] (seq c3)]
     (letfn [(g [i res c1 c2 c3]
               (cons res
                 (lazy-seq
                   (when-all-let [[h1 & t1] (seq c1)
                                  [h2 & t2] (seq c2)
                                  [h3 & t3] (seq c3)]
                     (g (inc i) (f res i h1 h2 h3) t1 t2 t3)))))]
       (g 1 (f init 0 init-h1 init-h2 init-h3) init-t1 init-t2 init-t3)))))

(s/fdef reductions-kv
  :args (s/or :three (s/cat :f (s/fspec :args (s/cat :result any?
                                                :index int?
                                                :value any?)
                                 :ret any?)
                       :init any?
                       :coll ::nilable-seq)
          :four (s/cat :f (s/fspec :args (s/cat :result any?
                                           :index int?
                                           :value1 any?
                                           :value2 any?)
                            :ret any?)
                  :init any?
                  :c1 ::nilable-seq
                  :c2 ::nilable-seq)
          :five (s/cat :f (s/fspec :args (s/cat :result any?
                                           :index int?
                                           :value1 any?
                                           :value2 any?
                                           :value3 any?)
                            :ret any?)
                  :init any?
                  :c1 ::nilable-seq
                  :c2 ::nilable-seq
                  :c3 ::nilable-seq))
  :ret any?)

(defn reduce-kv-with-stop
  "Reduces a sequence using stopping predicates. Function `f` and predicates
  take the result value, an index, and the item value(s)."
  ([f init coll {::keys [stop-pred1 err-pred1 err-return-fn1]}]
   (loop [i 0
          [h & t] coll
          res init]
     (cond
       (or (not h) (and stop-pred1 (stop-pred1 res i h))) res

       (and err-pred1 (err-pred1 res i h))
       (when err-return-fn1 (err-return-fn1 res i h))

       :else (recur (inc i) t (f res i h)))))
  ([f init c1 c2 {::keys [stop-pred2 err-pred2 err-return-fn2]}]
   (loop [i 0
          [h1 & t1] c1
          [h2 & t2] c2
          res init]
     (cond
       (or (not h1) (not h2) (and stop-pred2 (stop-pred2 res i h1 h2))) res

       (and err-pred2 (err-pred2 res i h1 h2))
       (when err-return-fn2 (err-return-fn2 res i h1 h2))

       :else (recur (inc i) t1 t2 (f res i h1 h2)))))
  ([f init c1 c2 c3 {::keys [stop-pred3 err-pred3 err-return-fn3]}]
   (loop [i 0
          [h1 & t1] c1
          [h2 & t2] c2
          [h3 & t3] c3 res init]
     (cond
       (or (not h1) (not h2) (not h3)
         (and stop-pred3 (stop-pred3 res i h1 h2 h3)))
       res

       (and err-pred3 (err-pred3 res i h1 h2 h3))
       (when err-return-fn3 (err-return-fn3 res i h1 h2 h3))

       :else (recur (inc i) t1 t2 t3 (f res i h1 h2 h3))))))

(s/def ::pred1
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value any?)
    :ret boolean?))

(s/def ::pred2
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value1 any?
                   :value2 any?)
    :ret boolean?))

(s/def ::pred3
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value1 any?
                   :value2 any?
                   :value3 any?)
    :ret boolean?))

(s/def ::f1
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value any?)
    :ret any?))

(s/def ::f2
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value1 any?
                   :value2 any?)
    :ret any?))

(s/def ::f3
  (s/fspec :args (s/cat :result any?
                   :index int?
                   :value1 any?
                   :value2 any?
                   :value3 any?)
    :ret any?))

(s/def ::stop-pred1 ::pred1)
(s/def ::err-pred1 ::pred1)
(s/def ::err-return-fn1 ::f1)
(s/def ::stop-pred2 ::pred2)
(s/def ::err-pred2 ::pred2)
(s/def ::err-return-fn2 ::f2)
(s/def ::stop-pred3 ::pred3)
(s/def ::err-pred3 ::pred3)
(s/def ::err-return-fn3 ::f3)

(s/fdef reduce-kv-with-stop
  :args (s/or :four (s/cat :f ::f1
                      :init any?
                      :coll ::nilable-seq
                      :args (s/keys :opt [::stop-pred1
                                          ::err-pred1
                                          ::err-return-fn1]))
          :five (s/cat :f ::f2
                  :init any?
                  :c1 ::nilable-seq
                  :c2 ::nilable-seq
                  :args (s/keys :opt [::stop-pred2
                                      ::err-pred2
                                      ::err-return-fn2]))
          :six (s/cat :f ::f3
                 :init any?
                 :c1 ::nilable-seq
                 :c2 ::nilable-seq
                 :c3 ::nilable-seq
                 :args (s/keys :opt [::stop-pred3
                                     ::err-pred3
                                     ::err-return-fn3])))
  :ret any?)
