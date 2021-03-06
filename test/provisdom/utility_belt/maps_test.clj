(ns provisdom.utility-belt.maps-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.maps :as maps]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;36 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest filter-map-test
  (is (spec-check maps/filter-map))
  (is= {:a 1}
       (maps/filter-map (fn [k v]
                          (and (number? v) (< v 2)))
                        {:a 1 :b 3})))

(deftest submap?-test
  (is (spec-check maps/submap?))
  (is (maps/submap? {:a 1 :b 3} {:a 1}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 1 :b 2}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 2})))

(deftest fmap-test
  (is= {:a 2 :b 4}
       (maps/fmap (fn [x]
                    (when (number? x)
                      (inc x)))
                  {:a 1 :b 3})))

#_(ost/unstrument)
