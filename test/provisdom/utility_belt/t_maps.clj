(ns provisdom.utility-belt.t-maps
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.maps :as maps]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest filter-map-test
  (is= {:a 1} (maps/filter-map (fn (< % 2)) {:a 1 :b 3})))

(deftest map->sorted-map-test
  (is= {:a "hello" :b "hi"} (maps/map->sorted-map {:b "hi" :a "hello"})))

(deftest submap?-test
  (is (maps/submap? {:a 1 :b 3} {:a 1}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 1 :b 2}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 2})))

(deftest fmap-test
  (is= {:a 2 :b 4} (maps/fmap inc {:a 1 :b 3})))

(defspec-test test-filter-map `maps/filter-map)
(defspec-test test-map->sorted-map `maps/map->sorted-map)
(defspec-test test-submap? `maps/submap?)
(defspec-test test-fmap `maps/fmap)

#_(ost/unstrument)
