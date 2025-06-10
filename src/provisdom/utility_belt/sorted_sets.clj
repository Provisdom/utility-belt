(ns provisdom.utility-belt.sorted-sets
  "Utilities for working with sorted sets and generating specs for them.
   Provides predicates and spec generators for sorted sets with custom comparators."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

(s/def ::comparator (s/fspec :args (s/or :two (s/cat :k1 any?
                                                :k2 any?)
                                     :one (s/cat :k1 any?))
                      :ret boolean?))

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
  (and (sorted-set? s) (= (seq s) (sort-by comparator s))))

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
   - min-count: The minimum number of elements in the set
   - gen-max: The maximum number of elements to generate in test data
   
   Example:
   ```clojure
   (s/def ::sorted-integers (sorted-set-of int? 1 10))
   ```"
  [pred min-count gen-max]
  (let [sform `(s/coll-of ~pred
                 :into #{}
                 :kind set?
                 :min-count ~min-count
                 :gen-max ~gen-max)
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
   - min-count: The minimum number of elements in the set
   - gen-max: The maximum number of elements to generate in test data
   
   Example:
   ```clojure
   ;; A set of strings sorted by length
   (s/def ::strings-by-length 
     (sorted-set-by-of string? #(compare (count %1) (count %2)) 1 10))
   ```"
  [pred comparator min-count gen-max]
  (let [sform `(s/coll-of ~pred
                 :into #{}
                 :kind set?
                 :min-count ~min-count
                 :gen-max ~gen-max)
        xform `(s/and (partial sorted-set-by? ~comparator) ~sform)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (sorted-set-by ~comparator))
          (s/gen ~sform)))))

