(ns provisdom.utility-belt.maps
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost])
  (:import (java.util Map)))

(defn filter-map
  "Returns map with keys that meet `pred`."
  [pred m]
  (select-keys m
               (for [[k v] m :when (pred k v)]
                 k)))

(s/fdef filter-map
        :args (s/cat :pred ::pred :m map?)
        :ret map?)

(defn map->sorted-map
  "Returns sorted map."
  [m]
  (apply sorted-map (flatten (into [] m))))

(s/fdef map->sorted-map
        :args (s/cat :m map?)
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
        :args (s/cat :f (s/fspec :args (s/cat :x any?) :ret any?)
                     :m map?)
        :ret map?)

