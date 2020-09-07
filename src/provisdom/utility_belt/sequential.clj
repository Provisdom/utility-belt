(ns provisdom.utility-belt.sequential
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

;;;SORTED LIST
(defn sorted-list?
  "Tests whether `l` is a sorted list."
  [l]
  (and (list? l) (= l (sort l))))

(defn sorted-list-by?
  "Tests whether `l` is a sorted list by."
  [comparator l]
  (and (list? l) (= l (sort comparator l))))

(defmacro sorted-list-of
  "This macro builds the spec for a sorted list."
  [pred & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform sorted-list?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp sort list*)
                  (s/gen ~sform)))))

(defmacro sorted-list-by-of
  "This macro builds the spec for a sorted list by."
  [pred comparator & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform (partial sorted-list-by? ~comparator))]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp (partial sort ~comparator) list*)
                  (s/gen ~sform)))))

;;SORTED VECTOR
(defn sorted-vector?
  "Tests whether `v` is a sorted vector."
  [v]
  (and (vector? v) (= v (sort v))))

(defn sorted-vector-by?
  "Tests whether `v` is a sorted vector by."
  [comparator v]
  (and (vector? v) (= v (sort comparator v))))

(defmacro sorted-vector-of
  "This macro builds the spec for a sorted vector."
  [pred & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform sorted-vector?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp vec sort)
                  (s/gen ~sform)))))

(defmacro sorted-vector-by-of
  "This macro builds the spec for a sorted list by."
  [pred comparator & opts]
  (let [sform `(s/coll-of ~pred ~@opts)
        xform `(s/and ~sform (partial sorted-vector-by? ~comparator))]
    `(s/with-gen
       ~xform
       #(gen/fmap (comp vec (partial sort ~comparator))
                  (s/gen ~sform)))))