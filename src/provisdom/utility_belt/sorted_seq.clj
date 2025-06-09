(ns provisdom.utility-belt.sorted-seq
  "Provides predicates and spec generators for sorted sequences (lists and vectors).
   Unlike sorted collections that maintain their order (like sorted-set, sorted-map),
   these sequences are point-in-time sorted and won't re-sort when elements are added.
   For self-sorting collections, use sorted-sets namespace instead."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

(s/def ::comparator (s/fspec :args (s/or :two (s/cat :k1 any?
                                                :k2 any?)
                                     :one (s/cat :k1 any?))
                      :ret boolean?))

;;;LIST -- SORTED
(defn list-sorted?
  "Tests whether `l` is a list sorted."
  [l]
  (and (list? l) (= l (sort l))))

(comment "documentation only"
  (s/fdef list-sorted?
    :args (s/cat :l any?)
    :ret boolean?))

(defn list-sorted-by?
  "Tests whether `l` is a list sorted by."
  [comparator l]
  (and (list? l) (= l (sort comparator l))))

(comment "documentation only"
  (s/fdef list-sorted-by?
    :args (s/cat :comparator ::comparator
            :l any?)
    :ret boolean?))

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

;;SORTED VECTOR
(defn vector-sorted?
  "Tests whether `v` is a vector sorted."
  [v]
  (and (vector? v) (= v (sort v))))

(comment "documentation only"
  (s/fdef vector-sorted?
    :args (s/cat :v any?)
    :ret boolean?))

(defn vector-sorted-by?
  "Tests whether `v` is a vector sorted by."
  [comparator v]
  (and (vector? v) (= v (sort comparator v))))

(comment "documentation only"
  (s/fdef vector-sorted-by?
    :args (s/cat :comparator ::comparator
            :v any?)
    :ret boolean?))

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
