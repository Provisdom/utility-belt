(ns provisdom.utility-belt.extensions
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(s/def ::nilable-seq (s/nilable (s/every any?)))

;;;MACROS
(defmacro if-all-let
  ([bindings then else]
   (reduce (fn [subform binding]
             `(if-let [~@binding] ~subform ~else))
           then (reverse (partition 2 bindings))))
  ([bindings then]
   `(if-all-let ~bindings ~then nil)))

(defmacro when-all-let
  [bindings then]
  `(if-all-let ~bindings ~then nil))

;;;FUNCTIONS
(defn update-in-with-not-found
  "'Updates' a value in a nested associative structure, where `ks` is a sequence of keys
  and `f` is a function that will take the old value and any supplied args and return
  the new value, and returns a new nested structure.
  If any key does not exist, `not-found` will be used as the old value.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks] f not-found & args]
  (if ks
    (assoc m k (apply update-in-with-not-found (get m k not-found) ks f not-found args))
    (assoc m k (apply f (get m k not-found) args))))

(s/fdef update-in-with-not-found
        :args (s/cat :m any?
                     :ks (s/coll-of any?)
                     :f fn?
                     :not-found any?
                     :args (s/* any?))
        :ret any?)

(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second etc.
  Difference from interleave is that all elements are consumed."
  ([] '())
  ([c1] (lazy-seq (if (sequential? c1) (seq c1) '())))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (cond (and s1 s2) (cons (first s1)
                               (cons (first s2)
                                     (interleave-all (rest s1) (rest s2))))
             s1 s1
             s2 s2))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (map seq (conj colls c2 c1))]
       (if (every? identity ss)
         (concat (map first ss) (apply interleave-all (map rest ss)))
         (let [ns (filter identity ss)]
           (concat (map first ns) (apply interleave-all (map rest ns)))))))))

(s/fdef interleave-all
        :args (s/cat :c1 (s/? ::nilable-seq)
                     :c2 (s/? ::nilable-seq)
                     :colls (s/* ::nilable-seq))
        :ret (s/every any?))

(defn reduce-kv-ext
  "Extension of clojure's 'reduce-kv'.
  First collection must be the shortest.
  Function f takes the result value, an index, and the item value(s)."
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
        :args (s/or :three (s/cat :f fn?
                                :init any?
                                :coll ::nilable-seq)
                    :four+ (s/cat :f fn?
                                 :init any?
                                 :c1 ::nilable-seq
                                 :c2 ::nilable-seq
                                 :c3 (s/? ::nilable-seq)))
        :ret any?)

(defn reductions-kv
  "Returns a lazy seq of a reduction with indices.
  Function f takes the result value, an index, and the item value(s)."
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
        :args (s/or :three (s/cat :f fn?
                                  :init any?
                                  :coll ::nilable-seq)
                    :four+ (s/cat :f fn?
                                  :init any?
                                  :c1 ::nilable-seq
                                  :c2 ::nilable-seq
                                  :c3 (s/? ::nilable-seq)))
        :ret any?)

(defn reduce-kv-with-stop
  "Reduces a sequence using stopping predicates.
  Function f and predicates take the result value, an index,
   and the item value(s)."
  ([f init coll stop-pred]
   (reduce-kv-with-stop f init coll stop-pred nil nil))
  ([f init c1 c2 stop-pred]
   (reduce-kv-with-stop f init c1 c2 stop-pred nil nil))
  ([f init coll stop-pred err-pred err-return-fn]
   (loop [i 0 [h & t] coll res init]
     (cond
       (or (not h) (stop-pred res i h)) res
       (and err-pred (err-pred res i h)) (err-return-fn res i h)
       :else (recur (inc i) t (f res i h)))))
  ([f init c1 c2 stop-pred err-pred err-return-fn]
   (loop [i 0 [h1 & t1] c1 [h2 & t2] c2 res init]
     (cond
       (or (not h1) (not h2) (stop-pred res i h1 h2)) res
       (and err-pred (err-pred res i h1 h2)) (err-return-fn res i h1 h2)
       :else (recur (inc i) t1 t2 (f res i h1 h2)))))
  ([f init c1 c2 c3 stop-pred err-pred err-return-fn]
   (loop [i 0 [h1 & t1] c1 [h2 & t2] c2 [h3 & t3] c3 res init]
     (cond
       (or (not h1) (not h2) (not h3) (stop-pred res i h1 h2 h3)) res
       (and err-pred (err-pred res i h1 h2 h3)) (err-return-fn res i h1 h2 h3)
       :else (recur (inc i) t1 t2 t3 (f res i h1 h2 h3))))))

(s/fdef reduce-kv-with-stop
        :args (s/or :four (s/cat :f fn?
                                 :init any?
                                 :coll ::nilable-seq
                                 :stop-pred fn?)
                    :five (s/cat :f fn?
                                 :init any?
                                 :c1 ::nilable-seq
                                 :c2 ::nilable-seq
                                 :stop-pred fn?)
                    :six (s/cat :f fn?
                                :init any?
                                :coll ::nilable-seq
                                :stop-pred fn?
                                :err-pred (s/nilable fn?)
                                :err-return-fn (s/nilable fn?))
                    :seven+ (s/cat :f fn?
                                   :init any?
                                   :c1 ::nilable-seq
                                   :c2 ::nilable-seq
                                   :c3 (s/? ::nilable-seq)
                                   :stop-pred fn?
                                   :err-pred (s/nilable fn?)
                                   :err-return-fn (s/nilable fn?)))
        :ret any?)
