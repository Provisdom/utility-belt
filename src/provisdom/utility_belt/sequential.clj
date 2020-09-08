(ns provisdom.utility-belt.sequential
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

;;;This namespace has predicates and spec-builders for lists and vectors that
;;;have been sorted. This sequences will not maintain their sorting once new
;;;elements have been added. For that, use sorted-sets (see `sets` ns).

;;;LIST -- SORTED
(defn list-sorted?
  "Tests whether `l` is a list sorted."
  [l]
  (and (list? l) (= l (sort l))))

(defn list-sorted-by?
  "Tests whether `l` is a list sorted by."
  [comparator l]
  (and (list? l) (= l (sort comparator l))))

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

(defn vector-sorted-by?
  "Tests whether `v` is a vector sorted by."
  [comparator v]
  (and (vector? v) (= v (sort comparator v))))

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