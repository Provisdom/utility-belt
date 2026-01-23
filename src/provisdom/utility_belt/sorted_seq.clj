(ns provisdom.utility-belt.sorted-seq
  "Provides predicates and spec generators for sorted sequences (lists and vectors). Unlike sorted
   collections that maintain their order (like `sorted-set`, `sorted-map`), these sequences are
   point-in-time sorted and won't re-sort when elements are added. For self-sorting collections,
   use the sorted-sets namespace instead.

   Note: This namespace defines [[sorted?]] which shadows `clojure.core/sorted?`. The core
   function tests if something is a sorted collection type (`sorted-set`, `sorted-map`). This
   namespace's [[sorted?]] tests if elements are in ascending order.

   Includes:
   - Predicates: [[sorted?]], [[strictly-sorted?]], [[sorted-desc?]], [[strictly-sorted-desc?]]
   - Type-specific: [[list-sorted?]], [[vector-sorted?]], [[seq-sorted?]] (and variants)
   - Binary search: [[binary-search]], [[binary-search-by]]
   - Utilities: [[insert-sorted]], [[merge-sorted]]"
  (:refer-clojure :exclude [sorted?])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

(def ^:private homogeneous-seqable-gen
  "Generator for seqables where all elements are mutually comparable."
  (gen/one-of [(gen/vector (gen/int))
               (gen/vector (gen/string-alphanumeric))
               (gen/vector (gen/keyword))
               (gen/list (gen/int))
               (gen/list (gen/string-alphanumeric))]))

(def ^:private universal-comparator-gen
  "Generator for comparators that work with any two values. Returns functions that impose an
   arbitrary but consistent ordering based on hash codes."
  (gen/return (fn [a b] (compare (hash a) (hash b)))))

(s/def ::comparator
  (s/with-gen
    (s/fspec :args (s/cat :a any? :b any?)
             :ret int?)
    (fn [] universal-comparator-gen)))

;;;HELPER FUNCTIONS
(defn- pairs-compare-satisfy?
  "Returns true if all adjacent pairs satisfy (sign-pred (compare a b)).
   Uses compare for general Comparable support. O(n) time complexity."
  [sign-pred coll]
  (or (empty? coll)
      (let [s (seq coll)]
        (loop [prev (first s)
               remaining (rest s)]
          (if (empty? remaining)
            true
            (let [curr (first remaining)]
              (if (sign-pred (compare prev curr))
                (recur curr (rest remaining))
                false)))))))

(defn- pairs-satisfy-by?
  "Returns true if all adjacent pairs satisfy (comparator a b) with given sign check.
   O(n) time complexity."
  [comparator sign-pred coll]
  (or (empty? coll)
      (let [s (seq coll)]
        (loop [prev (first s)
               remaining (rest s)]
          (if (empty? remaining)
            true
            (let [curr (first remaining)]
              (if (sign-pred (comparator prev curr))
                (recur curr (rest remaining))
                false)))))))

;;;GENERIC SORTED PREDICATES
(defn sorted?
  "Tests whether `coll` is sorted in ascending order (allows duplicates). Works on any Comparable
   elements. O(n) time complexity."
  [coll]
  (pairs-compare-satisfy? #(<= % 0) coll))

(s/fdef sorted?
  :args (s/with-gen (s/cat :coll seqable?)
          (fn [] (gen/fmap (fn [coll] [coll]) homogeneous-seqable-gen)))
  :ret boolean?)

(defn sorted-by?
  "Tests whether `coll` is sorted by the given `comparator` (allows duplicates). Comparator should
   return negative, zero, or positive int."
  [comparator coll]
  (pairs-satisfy-by? comparator #(<= % 0) coll))

(s/fdef sorted-by?
  :args (s/with-gen (s/cat :comparator ::comparator :coll seqable?)
          (fn [] (gen/tuple universal-comparator-gen homogeneous-seqable-gen)))
  :ret boolean?)

(defn strictly-sorted?
  "Tests whether `coll` is strictly sorted in ascending order (no duplicates). Works on any
   Comparable elements. O(n) time complexity."
  [coll]
  (pairs-compare-satisfy? #(< % 0) coll))

(s/fdef strictly-sorted?
  :args (s/with-gen (s/cat :coll seqable?)
          (fn [] (gen/fmap (fn [coll] [coll]) homogeneous-seqable-gen)))
  :ret boolean?)

(defn strictly-sorted-by?
  "Tests whether `coll` is strictly sorted by the given `comparator` (no duplicates). Comparator
   should return negative, zero, or positive int."
  [comparator coll]
  (pairs-satisfy-by? comparator #(< % 0) coll))

(s/fdef strictly-sorted-by?
  :args (s/with-gen (s/cat :comparator ::comparator :coll seqable?)
          (fn [] (gen/tuple universal-comparator-gen homogeneous-seqable-gen)))
  :ret boolean?)

(defn sorted-desc?
  "Tests whether `coll` is sorted in descending order (allows duplicates). Works on any Comparable
   elements. O(n) time complexity."
  [coll]
  (pairs-compare-satisfy? #(>= % 0) coll))

(s/fdef sorted-desc?
  :args (s/with-gen (s/cat :coll seqable?)
          (fn [] (gen/fmap (fn [coll] [coll]) homogeneous-seqable-gen)))
  :ret boolean?)

(defn sorted-desc-by?
  "Tests whether `coll` is sorted in descending order by the given `comparator`. Comparator should
   return negative, zero, or positive int."
  [comparator coll]
  (pairs-satisfy-by? comparator #(>= % 0) coll))

(s/fdef sorted-desc-by?
  :args (s/with-gen (s/cat :comparator ::comparator :coll seqable?)
          (fn [] (gen/tuple universal-comparator-gen homogeneous-seqable-gen)))
  :ret boolean?)

(defn strictly-sorted-desc?
  "Tests whether `coll` is strictly sorted in descending order (no duplicates). Works on any
   Comparable elements. O(n) time complexity."
  [coll]
  (pairs-compare-satisfy? #(> % 0) coll))

(s/fdef strictly-sorted-desc?
  :args (s/with-gen (s/cat :coll seqable?)
          (fn [] (gen/fmap (fn [coll] [coll]) homogeneous-seqable-gen)))
  :ret boolean?)

(defn strictly-sorted-desc-by?
  "Tests whether `coll` is strictly sorted in descending order by `comparator`. Comparator should
   return negative, zero, or positive int."
  [comparator coll]
  (pairs-satisfy-by? comparator #(> % 0) coll))

(s/fdef strictly-sorted-desc-by?
  :args (s/with-gen (s/cat :comparator ::comparator :coll seqable?)
          (fn [] (gen/tuple universal-comparator-gen homogeneous-seqable-gen)))
  :ret boolean?)

;;;SEQ -- SORTED
(defn seq-sorted?
  "Tests whether `coll` is a sequential and sorted in ascending order."
  [coll]
  (and (sequential? coll) (sorted? coll)))

(s/fdef seq-sorted?
  :args (s/with-gen (s/cat :coll any?)
          (fn [] (gen/fmap (fn [coll] [coll]) homogeneous-seqable-gen)))
  :ret boolean?)

(defn seq-sorted-by?
  "Tests whether `coll` is a sequential sorted by the given `comparator`."
  [comparator coll]
  (and (sequential? coll) (sorted-by? comparator coll)))

(s/fdef seq-sorted-by?
  :args (s/with-gen (s/cat :comparator ::comparator :coll any?)
          (fn [] (gen/tuple universal-comparator-gen homogeneous-seqable-gen)))
  :ret boolean?)

;;;LIST -- SORTED
(defn list-sorted?
  "Tests whether `l` is a sorted list. O(n) time complexity."
  [l]
  (and (list? l) (sorted? l)))

(s/fdef list-sorted?
  :args (s/with-gen (s/cat :l any?)
          (fn [] (gen/fmap (fn [coll] [(apply list coll)])
                   (gen/vector (gen/int)))))
  :ret boolean?)

(defn list-sorted-by?
  "Tests whether `l` is a list sorted by the given `comparator`. O(n) time complexity."
  [comparator l]
  (and (list? l) (sorted-by? comparator l)))

(s/fdef list-sorted-by?
  :args (s/with-gen (s/cat :comparator ::comparator :l any?)
          (fn [] (gen/tuple universal-comparator-gen
                   (gen/fmap (fn [v] (apply list v)) (gen/vector (gen/int))))))
  :ret boolean?)

(defmacro list-sorted-of
  "This macro builds the spec for a list sorted."
  [pred & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform list-sorted?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp sort list*)
          (s/gen ~sform)))))

(defmacro list-sorted-by-of
  "This macro builds the spec for a list sorted by."
  [pred comparator & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform (partial list-sorted-by? ~comparator))]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp (partial sort ~comparator) list*)
          (s/gen ~sform)))))

;;;VECTOR -- SORTED
(defn vector-sorted?
  "Tests whether `v` is a sorted vector. O(n) time complexity."
  [v]
  (and (vector? v) (sorted? v)))

(s/fdef vector-sorted?
  :args (s/with-gen (s/cat :v any?)
          (fn [] (gen/fmap (fn [v] [v]) (gen/vector (gen/int)))))
  :ret boolean?)

(defn vector-sorted-by?
  "Tests whether `v` is a vector sorted by the given `comparator`. O(n) time complexity."
  [comparator v]
  (and (vector? v) (sorted-by? comparator v)))

(s/fdef vector-sorted-by?
  :args (s/with-gen (s/cat :comparator ::comparator :v any?)
          (fn [] (gen/tuple universal-comparator-gen (gen/vector (gen/int)))))
  :ret boolean?)

(defmacro vector-sorted-of
  "This macro builds the spec for a vector sorted."
  [pred & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform vector-sorted?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp vec sort)
          (s/gen ~sform)))))

(defmacro vector-sorted-by-of
  "This macro builds the spec for a vector sorted by."
  [pred comparator & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform (partial vector-sorted-by? ~comparator))]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp vec (partial sort ~comparator))
          (s/gen ~sform)))))

;;;BINARY SEARCH
(defn binary-search
  "Binary search for `target` in a sorted indexed collection `coll`. Returns the index if found,
   or `nil` if not found. O(log n) time complexity. Requires `coll` to support `nth` and `count`."
  [coll target]
  (when (seq coll)
    (loop [low 0
           high (dec (count coll))]
      (when (<= low high)
        (let [mid (quot (+ low high) 2)
              mid-val (nth coll mid)]
          (cond
            (= mid-val target) mid
            (< (compare mid-val target) 0) (recur (inc mid) high)
            :else (recur low (dec mid))))))))

(s/fdef binary-search
  :args (s/with-gen (s/cat :coll (s/and indexed? sorted?) :target any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple (gen/return sv) (gen/int)))))))
  :ret (s/nilable nat-int?))

(defn binary-search-by
  "Binary search using a custom `comparator`. Returns the index if found, or `nil` if not found.
   O(log n) time complexity."
  [comparator coll target]
  (when (seq coll)
    (loop [low 0
           high (dec (count coll))]
      (when (<= low high)
        (let [mid (quot (+ low high) 2)
              mid-val (nth coll mid)
              cmp (comparator mid-val target)]
          (cond
            (zero? cmp) mid
            (neg? cmp) (recur (inc mid) high)
            :else (recur low (dec mid))))))))

(s/fdef binary-search-by
  :args (s/with-gen (s/cat :comparator ::comparator
                           :coll (s/and indexed? sorted?)
                           :target any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple universal-comparator-gen
                         (gen/return sv)
                         (gen/int)))))))
  :ret (s/nilable nat-int?))

(defn binary-search-insertion-point
  "Find the insertion point for `target` in a sorted indexed collection. Returns the index where
   `target` should be inserted to maintain sorted order. If `target` exists, returns the index of
   the first occurrence. O(log n) time complexity."
  [coll target]
  (if (empty? coll)
    0
    (loop [low 0
           high (count coll)]
      (if (< low high)
        (let [mid (quot (+ low high) 2)
              mid-val (nth coll mid)]
          (if (< (compare mid-val target) 0)
            (recur (inc mid) high)
            (recur low mid)))
        low))))

(s/fdef binary-search-insertion-point
  :args (s/with-gen (s/cat :coll (s/and indexed? sorted?) :target any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple (gen/return sv) (gen/int)))))))
  :ret nat-int?)

(defn binary-search-insertion-point-by
  "Find the insertion point using a custom `comparator`. O(log n) time complexity."
  [comparator coll target]
  (if (empty? coll)
    0
    (loop [low 0
           high (count coll)]
      (if (< low high)
        (let [mid (quot (+ low high) 2)
              mid-val (nth coll mid)]
          (if (neg? (comparator mid-val target))
            (recur (inc mid) high)
            (recur low mid)))
        low))))

(s/fdef binary-search-insertion-point-by
  :args (s/with-gen (s/cat :comparator ::comparator
                           :coll (s/and indexed? sorted?)
                           :target any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple universal-comparator-gen
                         (gen/return sv)
                         (gen/int)))))))
  :ret nat-int?)

;;;SORTED UTILITIES
(defn insert-sorted
  "Insert `x` into sorted vector `v` maintaining sorted order. O(n) time complexity due to vector
   construction."
  [v x]
  (let [idx (binary-search-insertion-point v x)]
    (into (conj (subvec v 0 idx) x) (subvec v idx))))

(s/fdef insert-sorted
  :args (s/with-gen (s/cat :v (s/and vector? sorted?) :x any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple (gen/return sv) (gen/int)))))))
  :ret (s/and vector? sorted?))

(defn insert-sorted-by
  "Insert `x` into sorted vector `v` using `comparator`, maintaining sorted order. O(n) time
   complexity due to vector construction."
  [comparator v x]
  (let [idx (binary-search-insertion-point-by comparator v x)]
    (into (conj (subvec v 0 idx) x) (subvec v idx))))

(s/fdef insert-sorted-by
  :args (s/with-gen (s/cat :comparator ::comparator
                           :v (s/and vector? sorted?)
                           :x any?)
          (fn [] (gen/bind (gen/vector (gen/int))
                   (fn [v]
                     (let [sv (vec (sort v))]
                       (gen/tuple universal-comparator-gen
                         (gen/return sv)
                         (gen/int)))))))
  :ret vector?)

(defn merge-sorted
  "Merge two sorted sequences into a single sorted vector. O(n + m) time complexity where n and m
   are the sizes of the inputs."
  [coll1 coll2]
  (loop [result []
         s1 (seq coll1)
         s2 (seq coll2)]
    (cond
      (nil? s1) (into result s2)
      (nil? s2) (into result s1)
      :else (let [v1 (first s1)
                  v2 (first s2)]
              (if (<= (compare v1 v2) 0)
                (recur (conj result v1) (next s1) s2)
                (recur (conj result v2) s1 (next s2)))))))

(s/fdef merge-sorted
  :args (s/with-gen (s/cat :coll1 (s/and seqable? sorted?)
                           :coll2 (s/and seqable? sorted?))
          (fn [] (gen/tuple (gen/fmap (comp vec sort) (gen/vector (gen/int)))
                   (gen/fmap (comp vec sort) (gen/vector (gen/int))))))
  :ret (s/and vector? sorted?))

(defn merge-sorted-by
  "Merge two sorted sequences using a `comparator` into a single sorted vector. O(n + m) time
   complexity."
  [comparator coll1 coll2]
  (loop [result []
         s1 (seq coll1)
         s2 (seq coll2)]
    (cond
      (nil? s1) (into result s2)
      (nil? s2) (into result s1)
      :else (let [v1 (first s1)
                  v2 (first s2)]
              (if (<= (comparator v1 v2) 0)
                (recur (conj result v1) (next s1) s2)
                (recur (conj result v2) s1 (next s2)))))))

(s/fdef merge-sorted-by
  :args (s/with-gen (s/cat :comparator ::comparator
                           :coll1 seqable?
                           :coll2 seqable?)
          (fn [] (gen/tuple universal-comparator-gen
                   (gen/fmap (comp vec sort) (gen/vector (gen/int)))
                   (gen/fmap (comp vec sort) (gen/vector (gen/int))))))
  :ret vector?)
