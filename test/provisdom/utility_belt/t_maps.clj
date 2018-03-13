(ns provisdom.utility-belt.t-maps
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

(deftest map->sorted-map-test
  (is (spec-check maps/map->sorted-map))
  (is= {:a "hello" :b "hi"} (maps/map->sorted-map {:b "hi" :a "hello"})))

(deftest submap?-test
  (is (spec-check maps/submap?))
  (is (maps/submap? {:a 1 :b 3} {:a 1}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 1 :b 2}))
  (is-not (maps/submap? {:a 1 :b 3} {:a 2})))

(deftest fmap-test
  (is (spec-check maps/fmap {:coll-check-limit 10
                             :coll-error-limit 10
                             :fspec-iterations 10
                             :recursion-limit  1
                             :test-check       {:num-tests 500}}))
  (is= {:a 2 :b 4} (maps/fmap #(when (number? %) (inc %)) {:a 1 :b 3})))

#_(ost/unstrument)
