(ns provisdom.utility-belt.maps
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.data.priority-map :refer [priority-map-keyfn-by]])
  (:import (java.util Map)
           (clojure.lang PersistentTreeMap)
           (clojure.data.priority_map PersistentPriorityMap)))

(defn priority-map?
  [m]
  (instance? PersistentPriorityMap m))

(defmacro priority-map-of
  "This macro builds the spec for a priority map built with
  [[clojure.data.priority-map/priority-map-keyfn-by]]. `keyfn` is the function
  to apply to the VALUE of the map for comparison. `comparator` is how to
  compare the outputs of the `keyfn`. A common example would be to use
  'identity' for the `keyfn` and '<' for the `comparator`."
  [kpred vpred keyfn comparator & opts]
  (let [sform `(s/map-of ~kpred ~vpred ~@opts)
        xform `(s/and ~sform priority-map?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (priority-map-keyfn-by ~keyfn ~comparator))
                  (s/gen ~sform)))))

(defn sorted-map?
  [m]
  (instance? PersistentTreeMap m))

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

(s/fdef fmap
        :args (s/cat :f (s/fspec :args (s/cat :x any?)
                                 :ret any?)
                     :m map?)
        :ret map?)

