(ns provisdom.utility-belt.maps-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.maps :as maps]))

;8 seconds

(set! *warn-on-reflection* true)

(def prio-map (apply maps/priority-map identity < {:a 1 :b 3}))

(deftest priority-map?-test
  (t/with-instrument `maps/priority-map?
    (is (t/spec-check maps/priority-map?)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (maps/priority-map? {:a 1 :b 3}))
    (is (maps/priority-map? prio-map))))

(deftest priority-map-test
  (t/is= {[:a 1] [:b 3]} prio-map))

(deftest sorted-map?-test
  (t/with-instrument `maps/sorted-map?
    (is (t/spec-check maps/sorted-map?)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (maps/sorted-map? {:b 2 :a 1}))
    (is (maps/sorted-map? (apply sorted-map [:b 2 :a 1])))))

(deftest sorted-map-by?-test
  (t/is-not (maps/sorted-map-by? > (apply sorted-map-by < [1 10 2 20])))
  (is (maps/sorted-map-by? > (apply sorted-map-by > [1 10 2 20]))))

(deftest sorted-map-monotonic?-test
  (t/with-instrument `maps/sorted-map-monotonic?
    (is (t/spec-check maps/sorted-map-monotonic?)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (maps/sorted-map-monotonic? (apply sorted-map [:b 1 :a 2])))
    (is (maps/sorted-map-monotonic? (apply sorted-map [:b 1 :a 1])))
    (is (maps/sorted-map-monotonic? (apply sorted-map [:b 2 :a 1])))))

(deftest sorted-map-strictly-monotonic?-test
  (t/with-instrument `maps/sorted-map-strictly-monotonic?
    (is (t/spec-check maps/sorted-map-strictly-monotonic?)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not
      (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 1 :a 2])))
    (t/is-not
      (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 1 :a 1])))
    (is (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 2 :a 1])))))

(deftest sorted-map-monotonic-by?-test
  (t/is-not (maps/sorted-map-monotonic-by?
            > > (apply sorted-map-by > [2 10 1 20])))
  (is (maps/sorted-map-monotonic-by?
        > > (apply sorted-map-by > [2 10 1 10])))
  (is (maps/sorted-map-monotonic-by?
        > > (apply sorted-map-by > [2 20 1 10])))
  (t/is-not (maps/sorted-map-monotonic-by?
            > > (apply sorted-map-by < [2 20 1 10]))))

(deftest sorted-map-strictly-monotonic-by?-test
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
            > > (apply sorted-map-by > [2 10 1 20])))
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
            > > (apply sorted-map-by > [2 10 1 10])))
  (is (maps/sorted-map-strictly-monotonic-by?
        > > (apply sorted-map-by > [2 20 1 10])))
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
            > > (apply sorted-map-by < [2 20 1 10]))))

(deftest filter-map-test
  (t/with-instrument `maps/filter-map
    (is (t/spec-check maps/filter-map)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= {:a 1}
      (maps/filter-map (fn [k v]
                         (and (number? v) (< v 2)))
        {:a 1 :b 3}))))

(deftest submap?-test
  (t/with-instrument `maps/filter-map
    (is (t/spec-check maps/filter-map)))
  (t/with-instrument (st/instrumentable-syms)
    (is (maps/submap? {:a 1 :b 3} {:a 1}))
    (t/is-not (maps/submap? {:a 1 :b 3} {:a 1 :b 2}))
    (t/is-not (maps/submap? {:a 1 :b 3} {:a 2}))))

(deftest fmap-test
  (t/is= {:a 2 :b 4}
    (maps/fmap (fn [x]
                 (when (number? x)
                   (inc x)))
      {:a 1 :b 3})))
