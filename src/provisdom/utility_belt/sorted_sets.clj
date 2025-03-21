(ns provisdom.utility-belt.sorted-sets
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
  "This macro builds the spec for a sorted set."
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
  "This macro builds the spec for a sorted set by."
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

