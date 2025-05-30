(ns provisdom.utility-belt.maps
  (:require
    [clojure.data.priority-map :refer [priority-map-keyfn-by]]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.utility-belt.gen-helper :as genh])
  (:import (clojure.data.priority_map PersistentPriorityMap)
           (clojure.lang PersistentTreeMap)
           (java.util Map)))

(declare priority-map? priority-map)

(s/def ::priority-map
  (s/with-gen priority-map?
    #(gen/one-of [(priority-map identity < 1 2 3 4)])))

(s/def ::comparator (s/fspec :args (s/or :two (s/cat :k1 any?
                                                :k2 any?)
                                     :one (s/cat :k1 any?))
                      :ret boolean?))

;;;PRIORITY MAP
(defn priority-map?
  [m]
  (instance? PersistentPriorityMap m))

(s/fdef priority-map?
  :args (s/cat :m any?)
  :ret boolean?)

(defmacro priority-map-of
  "This macro builds the spec for a priority map built with [[priority-map]].
  `keyfn` is the function to apply to the VALUE of the map for comparison.
  `comparator` is how to compare the outputs of the `keyfn`. A common example
  would be to use 'identity' for the `keyfn` and '<' for the `comparator`."
  [kpred vpred keyfn comparator & opts]
  (let [sform `(s/map-of ~kpred ~vpred ~@opts)
        xform `(s/and ~sform priority-map?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (priority-map-keyfn-by ~keyfn ~comparator))
          (s/gen ~sform)))))

(defn priority-map
  "Creates a [[clojure.data.priority-map/priority-map-keyfn-by]]. A common
  example would be to use 'identity' for the `keyfn` and '<' for the
  `comparator`."
  [keyfn comparator & keyvals]
  (apply priority-map-keyfn-by keyfn comparator keyvals))

(comment "documentation only"
  (s/fdef priority-map
    :args (s/cat :keyfn (s/fspec :args (s/cat :value any?)
                          :ret any?)
            :comparator ::comparator
            :keyvals (s/* any?))
    :ret boolean?))

;;;SORTED MAP
(defn sorted-map?
  [m]
  (instance? PersistentTreeMap m))

(s/fdef sorted-map?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-by?
  "Tests whether `m` is a sorted map by."
  [comparator m]
  (and (sorted-map? m)
    (= (keys m) (sort comparator (keys m)))))

(comment "documentation only"
  (s/fdef sorted-map-by?
    :args (s/cat :comparator ::comparator
            :m any?)
    :ret boolean?))

(defmacro sorted-map-of
  "This macro builds the spec for a sorted map."
  ([kpred vpred & opts]
   (let [sform `(s/map-of ~kpred ~vpred ~@opts)
         xform `(s/and sorted-map? ~sform)]
     `(s/with-gen
        ~xform
        #(gen/fmap (partial into (sorted-map))
           (s/gen ~sform))))))

(defmacro sorted-map-by-of
  "This macro builds the spec for a sorted map by."
  [kpred vpred comparator & opts]
  (let [sform `(s/map-of ~kpred ~vpred ~@opts)
        xform `(s/and (partial sorted-map-by? ~comparator) ~sform)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (sorted-map-by ~comparator))
          (s/gen ~sform)))))

;;;SORTED MAP MONOTONIC
(defn sorted-map-monotonic?
  "Tests whether `m` is a 'sorted map monotonic'. A 'sorted map monotonic' is a
  sorted map (by keys) that has values that are monotonic."
  [m]
  (and (sorted-map? m)
    (or (empty? m)
      (let [v (vals m)]
        (= v (sort v))))))

(s/fdef sorted-map-monotonic?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-strictly-monotonic?
  "Tests whether `m` is a 'sorted map strictly monotonic'. A 'sorted map
  strictly monotonic' is a sorted map (by keys) that has values that are
  strictly monotonic."
  [m]
  (and (sorted-map-monotonic? m) (apply distinct? (vals m))))

(s/fdef sorted-map-strictly-monotonic?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-monotonic-by?
  "Tests whether `m` is a sorted map monotonic by."
  [comparator-k comparator-v m]
  (and (sorted-map-by? comparator-k m)
    (or (empty? m)
      (let [v (vals m)]
        (= v (sort comparator-v v))))))

(comment "documentation only"
  (s/fdef sorted-map-monotonic-by?
    :args (s/cat :comparator-k ::comparator
            :comparator-v ::comparator
            :m any?)
    :ret boolean?))

(defn sorted-map-strictly-monotonic-by?
  "Tests whether `m` is a map monotonic by."
  [comparator-k comparator-v m]
  (and (sorted-map-monotonic-by? comparator-k comparator-v m)
    (apply distinct? (vals m))))

(comment "documentation only"
  (s/fdef sorted-map-strictly-monotonic-by?
    :args (s/cat :comparator-k ::comparator
            :comparator-v ::comparator
            :m any?)
    :ret boolean?))

(defmacro sorted-map-monotonic-of
  "This macro builds the spec for a sorted map monotonic."
  [kpred vpred min-count gen-max strictly?]
  (let [sform `(s/map-of ~kpred ~vpred
                 :min-count ~min-count)
        xform `(if ~strictly?
                 (s/and sorted-map-strictly-monotonic? ~sform)
                 (s/and sorted-map-monotonic? ~sform))]
    `(s/with-gen
       ~xform
       #(genh/gen-let
          [i# (s/gen (s/int-in ~min-count (inc ~gen-max)))
           ks# (s/gen (s/coll-of ~kpred
                        :count i#
                        :distinct true))
           vs# (if ~strictly?
                 (s/gen (s/coll-of ~vpred
                          :count i#
                          :distinct true))
                 (s/gen (s/coll-of ~vpred
                          :count i#)))]
          (into (sorted-map) (zipmap (sort ks#) (sort vs#)))))))

(defmacro sorted-map-monotonic-by-of
  "This macro builds the spec for a sorted map monotonic by."
  [kpred vpred comparator-k comparator-v min-count gen-max strictly?]
  (let [sform `(s/map-of ~kpred ~vpred
                 :min-count ~min-count)
        xform `(if ~strictly?
                 (s/and
                   (partial sorted-map-strictly-monotonic-by?
                     ~comparator-k
                     ~comparator-v)
                   ~sform)
                 (s/and (partial sorted-map-monotonic-by?
                          ~comparator-k
                          ~comparator-v)
                   ~sform))]
    `(s/with-gen
       ~xform
       #(genh/gen-let
          [i# (s/gen (s/int-in ~min-count (inc ~gen-max)))
           ks# (s/gen (s/coll-of ~kpred
                        :count i#
                        :distinct true))
           vs# (if ~strictly?
                 (s/gen (s/coll-of ~vpred
                          :count i#
                          :distinct true))
                 (s/gen (s/coll-of ~vpred
                          :count i#)))]
          (into (sorted-map-by ~comparator-k)
            (zipmap
              (sort-by ~comparator-k ks#) (sort-by ~comparator-v vs#)))))))

;;MAP MANIPULATION
(defn filter-map
  "Returns map with keys that meet `pred`, which takes a key and value as
  inputs."
  [pred-kv m]
  (select-keys m
    (for [[k v] m :when (pred-kv k v)]
      k)))

(s/fdef filter-map
  :args (s/cat :pred-kv (s/fspec :args (s/cat :k any? :v any?)
                          :ret boolean?)
          :m map?)
  :ret map?)

(defn submap?
  "Checks whether m contains all entries in `sub`."
  [m sub]
  (.containsAll (.entrySet ^Map m)
    (.entrySet ^Map sub)))

(s/fdef submap?
  :args (s/cat :m map? :sub map?)
  :ret boolean?)

(defn fmap
  "Maps a function onto the values of a map."
  [f m]
  (into (empty m)
    (for [[k v] m]
      [k (f v)])))

(comment "documentation only -- causes instrumentation difficulties for users"
  (s/fdef fmap
    :args (s/cat :f (s/fspec :args (s/cat :x any?)
                      :ret any?)
            :m map?)
    :ret map?))

