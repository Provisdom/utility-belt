(ns provisdom.utility-belt.extensions
  "Extended functionality for Clojure's core data operations.
   Provides macros for conditional binding and functions for working with
   collections, including nested updates, interleaving, reducing with indices,
   and common collection utilities like distinct-by, index-of, and safe-nth."
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::nilable-seq (s/nilable (s/every any?)))

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

(defmacro if-some-let
  "A variant of if-let that tests multiple bindings, requiring all to be non-nil.

   Unlike if-all-let which checks for truthiness, this only checks for nil.
   This allows false values to pass through, which is useful when false is a
   valid value in your domain.

   Parameters:
   - bindings: A vector of binding forms (as in let)
   - then: Expression to evaluate if all bindings are non-nil
   - else: Expression to evaluate if any binding is nil (optional, defaults to nil)

   Example:
   ```clojure
   (if-some-let [enabled? (get-setting :enabled)  ; false is valid
                 config (get-config)]
     (process enabled? config)
     (use-defaults))
   ```"
  ([bindings then else]
   (reduce (fn [subform binding]
             `(if-some [~@binding] ~subform ~else))
     then
     (reverse (partition 2 bindings))))
  ([bindings then]
   `(if-some-let ~bindings ~then nil)))

(defmacro when-some-let
  "A variant of when-let that tests multiple bindings, requiring all to be non-nil.

   Unlike when-all-let which checks for truthiness, this only checks for nil.
   This allows false values to pass through.
   A simplified version of if-some-let without the else clause.

   Parameters:
   - bindings: A vector of binding forms (as in let)
   - then: Expression to evaluate if all bindings are non-nil

   Example:
   ```clojure
   (when-some-let [enabled? (get-setting :enabled)  ; false is valid
                   config (get-config)]
     (process enabled? config))
   ```"
  [bindings then]
  `(if-some-let ~bindings ~then nil))

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

;;;ADDITIONAL COLLECTION UTILITIES
(defn deep-merge
  "Recursively merges maps. If values at the same key are both maps, they are
   merged recursively. Otherwise, the value from the last map wins.

   Parameters:
   - maps: Variable number of maps to merge

   Examples:
   ```clojure
   (deep-merge {:a {:b 1}} {:a {:c 2}})
   ;; => {:a {:b 1 :c 2}}

   (deep-merge {:a {:b 1}} {:a {:b 2}})
   ;; => {:a {:b 2}}

   (deep-merge {:a 1} {:a {:b 2}})
   ;; => {:a {:b 2}}
   ```"
  ([] {})
  ([m] m)
  ([m1 m2]
   (if (and (map? m1) (map? m2))
     (merge-with deep-merge m1 m2)
     m2))
  ([m1 m2 & maps]
   (reduce deep-merge (deep-merge m1 m2) maps)))

(comment "documentation only -- merge-with calls deep-merge recursively with non-map values"
  (s/fdef deep-merge
    :args (s/cat :maps (s/* map?))
    :ret map?))

(defn index-by
  "Returns a map of the elements of `coll` keyed by the result of `f` on each
   element. Similar to `group-by` but assumes unique keys.

   Parameters:
   - f: A function to apply to each element to get its key
   - coll: The collection to index

   Examples:
   ```clojure
   (index-by :id [{:id 1 :name \"a\"} {:id 2 :name \"b\"}])
   ;; => {1 {:id 1 :name \"a\"} 2 {:id 2 :name \"b\"}}
   ```"
  [f coll]
  (persistent!
    (reduce (fn [acc x]
              (assoc! acc (f x) x))
      (transient {})
      coll)))

(s/fdef index-by
  :args (s/cat :f (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret map?)

(defn frequencies-by
  "Returns a map from the result of applying `f` to each element of `coll` to the count of elements
  that produced that result. Like `frequencies` but applies `f` first.

   Parameters:
   - f: A function to apply to each element
   - coll: The collection to count

   Examples:
   ```clojure
   (frequencies-by :type [{:type :a} {:type :b} {:type :a}])
   ;; => {:a 2 :b 1}

   (frequencies-by even? [1 2 3 4 5])
   ;; => {false 3 true 2}
   ```"
  [f coll]
  (persistent!
    (reduce (fn [acc x]
              (let [k (f x)]
                (assoc! acc k (inc (get acc k 0)))))
      (transient {})
      coll)))

(s/fdef frequencies-by
  :args (s/cat :f (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/map-of any? pos-int?))

(defn partition-map
  "Partitions `coll` into two vectors based on a predicate.
   Returns `[matches non-matches]` where `matches` contains elements for which
   `pred` returns truthy and `non-matches` contains the rest.

   More efficient than calling `filter` and `remove` separately.

   Parameters:
   - pred: A predicate function
   - coll: The collection to partition

   Examples:
   ```clojure
   (partition-map even? [1 2 3 4 5])
   ;; => [[2 4] [1 3 5]]

   (partition-map :active? [{:active? true} {:active? false}])
   ;; => [[{:active? true}] [{:active? false}]]
   ```"
  [pred coll]
  (reduce (fn [[matches non-matches] x]
            (if (pred x)
              [(conj matches x) non-matches]
              [matches (conj non-matches x)]))
    [[] []]
    coll))

(s/fdef partition-map
  :args (s/cat :pred (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/tuple vector? vector?))

(defn find-first
  "Returns the first element in `coll` for which `pred` returns truthy, or nil if no such element
  exists. Unlike `(first (filter pred coll))`, this is eager and stops as soon as a match is found.

   Parameters:
   - pred: A predicate function
   - coll: The collection to search

   Examples:
   ```clojure
   (find-first even? [1 3 4 5 6])
   ;; => 4

   (find-first neg? [1 2 3])
   ;; => nil
   ```"
  [pred coll]
  (reduce (fn [_ x]
            (when (pred x)
              (reduced x)))
    nil
    coll))

(s/fdef find-first
  :args (s/cat :pred (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret any?)

(defn dissoc-in
  "Dissociates a key at a nested path in an associative structure.

   Removes the key at the end of the path. If removing a key results in an
   empty map at a parent level, that empty map is also removed, recursively
   up the path.

   Parameters:
   - m: The nested associative structure
   - ks: A sequence of keys representing the path to the key to remove

   Examples:
   ```clojure
   (dissoc-in {:a {:b 1 :c 2}} [:a :b])
   ;; => {:a {:c 2}}

   (dissoc-in {:a {:b 1}} [:a :b])
   ;; => {}  ; empty nested map is removed

   (dissoc-in {:a {:b {:c 1}}} [:a :b :c])
   ;; => {}  ; all empty parents removed
   ```"
  [m [k & ks]]
  (if ks
    (let [v (get m k)]
      (if (map? v)
        (let [newmap (dissoc-in v ks)]
          (if (seq newmap)
            (assoc m k newmap)
            (dissoc m k)))
        m))
    (dissoc m k)))

(s/fdef dissoc-in
  :args (s/cat :m map?
          :ks (s/coll-of any? :min-count 1))
  :ret map?)

(defn assoc-some
  "Like assoc but skips key-value pairs where the value is nil.

   Useful for conditionally adding keys to a map without cluttering the code with when/if checks.

   Parameters:
   - m: The map to assoc into
   - kvs: Key-value pairs to potentially add

   Examples:
   ```clojure
   (assoc-some {:a 1} :b 2 :c nil :d 3)
   ;; => {:a 1 :b 2 :d 3}

   (assoc-some {} :name (get-name) :email (get-email))
   ;; Only includes keys with non-nil values
   ```"
  [m & kvs]
  (reduce (fn [acc [k v]]
            (if (some? v)
              (assoc acc k v)
              acc))
    m
    (partition 2 kvs)))

(s/fdef assoc-some
  :args (s/cat :m map?
          :kvs (s/* (s/cat :k any? :v any?)))
  :ret map?)

(defn distinct-by
  "Returns a lazy sequence of elements with duplicates removed, where duplicates
   are determined by the result of applying key-fn to each element.

   Like `distinct` but uses a key function to determine uniqueness.
   Preserves the first element seen for each key.

   Parameters:
   - key-fn: A function to apply to each element to get its uniqueness key
   - coll: The collection to process

   Examples:
   ```clojure
   (distinct-by :id [{:id 1 :name \"a\"} {:id 2 :name \"b\"} {:id 1 :name \"c\"}])
   ;; => ({:id 1 :name \"a\"} {:id 2 :name \"b\"})

   (distinct-by first [[1 :a] [2 :b] [1 :c] [3 :d]])
   ;; => ([1 :a] [2 :b] [3 :d])
   ```"
  [key-fn coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                 ((fn [[x :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [k (key-fn x)]
                        (if (contains? seen k)
                          (recur (rest s) seen)
                          (cons x (step (rest s) (conj seen k)))))))
                  xs seen)))]
    (step coll #{})))

(s/fdef distinct-by
  :args (s/cat :key-fn (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/every any?))

(defn index-of
  "Returns the index of the first element in coll for which pred returns truthy,
   or nil if no such element exists.

   Parameters:
   - pred: A predicate function
   - coll: The collection to search

   Examples:
   ```clojure
   (index-of even? [1 3 4 5 6])
   ;; => 2

   (index-of neg? [1 2 3])
   ;; => nil

   (index-of #(= % :target) [:a :b :target :c])
   ;; => 2
   ```"
  [pred coll]
  (loop [i 0
         s (seq coll)]
    (when s
      (if (pred (first s))
        i
        (recur (inc i) (next s))))))

(s/fdef index-of
  :args (s/cat :pred (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/nilable nat-int?))

(defn safe-nth
  "Like nth but returns not-found (default nil) for out-of-bounds indices
   instead of throwing an exception.

   Also handles negative indices gracefully by returning not-found.

   Parameters:
   - coll: A collection supporting nth
   - i: The index to retrieve
   - not-found: Value to return if index is out of bounds (default nil)

   Examples:
   ```clojure
   (safe-nth [1 2 3] 1)
   ;; => 2

   (safe-nth [1 2 3] 10)
   ;; => nil

   (safe-nth [1 2 3] 10 :missing)
   ;; => :missing

   (safe-nth [1 2 3] -1)
   ;; => nil
   ```"
  ([coll i]
   (safe-nth coll i nil))
  ([coll i not-found]
   (if (and (int? i)
            (>= i 0)
            (< i (count coll)))
     (nth coll i)
     not-found)))

(s/fdef safe-nth
  :args (s/cat :coll (s/every any?)
          :i int?
          :not-found (s/? any?))
  :ret any?)

(defn keep-kv
  "Returns a lazy sequence of non-nil results of applying f to the index and each element of coll.

   Like keep-indexed but with argument order (index, value) matching reduce-kv.

   Parameters:
   - f: A function of two arguments (index, value) returning a value or nil
   - coll: The collection to process

   Examples:
   ```clojure
   (keep-kv (fn [i x] (when (even? i) x)) [:a :b :c :d :e])
   ;; => (:a :c :e)

   (keep-kv (fn [i x] (when (> x 0) [i x])) [-1 2 -3 4])
   ;; => ([1 2] [3 4])
   ```"
  [f coll]
  (keep-indexed (fn [i x] (f i x)) coll))

(s/fdef keep-kv
  :args (s/cat :f (s/fspec :args (s/cat :index nat-int? :value any?)
                    :ret any?)
          :coll (s/every any?))
  :ret (s/every any?))

(defn take-until
  "Returns a lazy sequence of successive items from coll while pred returns
   truthy, including the first item for which pred returns falsey.

   Unlike take-while which excludes the terminating element, take-until includes it.

   Parameters:
   - pred: A predicate function
   - coll: The collection to process

   Examples:
   ```clojure
   (take-until #(< % 5) [1 2 3 5 6 7])
   ;; => (1 2 3 5)  ; includes 5, the first >= 5

   (take-while #(< % 5) [1 2 3 5 6 7])
   ;; => (1 2 3)    ; compare with take-while
   ```"
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [x (first s)]
        (cons x (when (pred x)
                  (take-until pred (rest s))))))))

(s/fdef take-until
  :args (s/cat :pred (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/every any?))

(defn drop-until
  "Returns a lazy sequence of items from coll starting from the first item
   for which pred returns falsey.

   Unlike drop-while which excludes the terminating element from the result,
   drop-until includes it as the first element of the returned sequence.

   Parameters:
   - pred: A predicate function
   - coll: The collection to process

   Examples:
   ```clojure
   (drop-until #(< % 5) [1 2 3 5 6 7])
   ;; => (5 6 7)  ; starts with first >= 5

   (drop-while #(< % 5) [1 2 3 5 6 7])
   ;; => (5 6 7)  ; same result in this case
   ```"
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [x (first s)]
        (if (pred x)
          (drop-until pred (rest s))
          s)))))

(s/fdef drop-until
  :args (s/cat :pred (s/fspec :args (s/cat :x any?) :ret any?)
          :coll (s/every any?))
  :ret (s/every any?))
