(ns provisdom.utility-belt.sorted-sets
  "Utilities for working with sorted sets and generating specs for them.
   Provides predicates and spec generators for sorted sets with custom comparators."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

;; A comparator function that takes two arguments and returns either:
;; - an int (negative, zero, positive) like `compare`
;; - a boolean like `<` or `>`
;; Both forms are accepted by Clojure's sorting functions.
(s/def ::comparator
  (s/fspec :args (s/cat :a any? :b any?)
           :ret (s/or :int int? :bool boolean?)))

;;;SORTED SET
(defn sorted-set?
  "Tests whether `s` is a sorted set."
  [s]
  (and (set? s) (sorted? s)))

(s/fdef sorted-set?
  :args (s/cat :s any?)
  :ret boolean?)

(defn sorted-set-by?
  "Tests whether `s` is a sorted set by."
  [comparator s]
  (and (sorted-set? s) (= (seq s) (sort comparator s))))

(comment "documentation only"
  (s/fdef sorted-set-by?
    :args (s/cat :comparator ::comparator
            :s any?)
    :ret boolean?))

(defmacro sorted-set-of
  "Creates a spec for a sorted set with natural ordering.

   Generates and validates sorted sets whose elements conform to the given predicate.
   The generated sets will maintain their natural sort order.

   Parameters:
   - pred: The spec/predicate that each element must satisfy
   - opts: Options passed to s/coll-of (:min-count, :max-count, :gen-max, etc.)

   Example:
   ```clojure
   (s/def ::sorted-integers (sorted-set-of int? :min-count 1 :gen-max 10))
   ```"
  [pred & opts]
  (let [sform `(s/coll-of ~pred :into #{} :kind set? ~@opts)
        xform `(s/and sorted-set? ~sform)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (sorted-set))
          (s/gen ~sform)))))

(defmacro sorted-set-by-of
  "Creates a spec for a sorted set with a custom comparator function.

   Generates and validates sorted sets whose elements conform to the given predicate
   and are ordered according to the provided comparator function.

   Parameters:
   - pred: The spec/predicate that each element must satisfy
   - comparator: The comparison function to use for ordering elements
   - opts: Options passed to s/coll-of (:min-count, :max-count, :gen-max, etc.)

   Example:
   ```clojure
   ;; A set of strings sorted by length
   (s/def ::strings-by-length
     (sorted-set-by-of string? #(compare (count %1) (count %2)) :min-count 1 :gen-max 10))
   ```"
  [pred comparator & opts]
  (let [sform `(s/coll-of ~pred :into #{} :kind set? ~@opts)
        xform `(s/and (partial sorted-set-by? ~comparator) ~sform)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (sorted-set-by ~comparator))
          (s/gen ~sform)))))

;;;NAVIGABLE SET OPERATIONS
(defn floor
  "Returns the greatest element in `sorted-set` that is <= `val`, or nil if none exists.
   Like Java's NavigableSet.floor()."
  [sorted-set val]
  (first (rsubseq sorted-set <= val)))

(s/fdef floor
  :args (s/cat :sorted-set sorted-set? :val any?)
  :ret any?)

(defn ceiling
  "Returns the least element in `sorted-set` that is >= `val`, or nil if none exists.
   Like Java's NavigableSet.ceiling()."
  [sorted-set val]
  (first (subseq sorted-set >= val)))

(s/fdef ceiling
  :args (s/cat :sorted-set sorted-set? :val any?)
  :ret any?)

(defn lower
  "Returns the greatest element in `sorted-set` that is < `val`, or nil if none exists.
   Like Java's NavigableSet.lower()."
  [sorted-set val]
  (first (rsubseq sorted-set < val)))

(s/fdef lower
  :args (s/cat :sorted-set sorted-set? :val any?)
  :ret any?)

(defn higher
  "Returns the least element in `sorted-set` that is > `val`, or nil if none exists.
   Like Java's NavigableSet.higher()."
  [sorted-set val]
  (first (subseq sorted-set > val)))

(s/fdef higher
  :args (s/cat :sorted-set sorted-set? :val any?)
  :ret any?)

(defn subset
  "Returns elements in `sorted-set` in the range [from, to).
   Like Java's NavigableSet.subSet(). Returns a lazy seq."
  [sorted-set from to]
  (subseq sorted-set >= from < to))

(s/fdef subset
  :args (s/cat :sorted-set sorted-set? :from any? :to any?)
  :ret seqable?)

(defn subset-inclusive
  "Returns elements in `sorted-set` in the range [from, to].
   Like Java's NavigableSet.subSet(from, true, to, true). Returns a lazy seq."
  [sorted-set from to]
  (subseq sorted-set >= from <= to))

(s/fdef subset-inclusive
  :args (s/cat :sorted-set sorted-set? :from any? :to any?)
  :ret seqable?)

