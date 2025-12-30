(ns provisdom.utility-belt.maps-test
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.maps :as maps]))

;11 seconds

(set! *warn-on-reflection* true)

(def prio-map (apply maps/priority-map identity < {:a 1 :b 3}))

(t/deftest priority-map?-test
  (t/with-instrument `maps/priority-map?
    (t/is-spec-check maps/priority-map?))
  (t/with-instrument :all
    (t/is-not (maps/priority-map? {:a 1 :b 3}))
    (t/is (maps/priority-map? prio-map))))

(t/deftest priority-map-test
  (t/is= {[:a 1] [:b 3]} prio-map))

(t/deftest sorted-map?-test
  (t/with-instrument `maps/sorted-map?
    (t/is-spec-check maps/sorted-map?))
  (t/with-instrument :all
    (t/is-not (maps/sorted-map? {:b 2 :a 1}))
    (t/is (maps/sorted-map? (apply sorted-map [:b 2 :a 1])))))

(t/deftest sorted-map-by?-test
  (t/is-not (maps/sorted-map-by? > (apply sorted-map-by < [1 10 2 20])))
  (t/is (maps/sorted-map-by? > (apply sorted-map-by > [1 10 2 20]))))

(t/deftest sorted-map-monotonic?-test
  (t/with-instrument `maps/sorted-map-monotonic?
    (t/is-spec-check maps/sorted-map-monotonic?))
  (t/with-instrument :all
    (t/is-not (maps/sorted-map-monotonic? (apply sorted-map [:b 1 :a 2])))
    (t/is (maps/sorted-map-monotonic? (apply sorted-map [:b 1 :a 1])))
    (t/is (maps/sorted-map-monotonic? (apply sorted-map [:b 2 :a 1])))))

(t/deftest sorted-map-strictly-monotonic?-test
  (t/with-instrument `maps/sorted-map-strictly-monotonic?
    (t/is-spec-check maps/sorted-map-strictly-monotonic?))
  (t/with-instrument :all
    (t/is-not
      (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 1 :a 2])))
    (t/is-not
      (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 1 :a 1])))
    (t/is (maps/sorted-map-strictly-monotonic? (apply sorted-map [:b 2 :a 1])))))

(t/deftest sorted-map-monotonic-by?-test
  (t/is-not (maps/sorted-map-monotonic-by?
              > > (apply sorted-map-by > [2 10 1 20])))
  (t/is (maps/sorted-map-monotonic-by?
          > > (apply sorted-map-by > [2 10 1 10])))
  (t/is (maps/sorted-map-monotonic-by?
          > > (apply sorted-map-by > [2 20 1 10])))
  (t/is-not (maps/sorted-map-monotonic-by?
              > > (apply sorted-map-by < [2 20 1 10]))))

(t/deftest sorted-map-strictly-monotonic-by?-test
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
              > > (apply sorted-map-by > [2 10 1 20])))
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
              > > (apply sorted-map-by > [2 10 1 10])))
  (t/is (maps/sorted-map-strictly-monotonic-by?
          > > (apply sorted-map-by > [2 20 1 10])))
  (t/is-not (maps/sorted-map-strictly-monotonic-by?
              > > (apply sorted-map-by < [2 20 1 10]))))

(t/deftest filter-map-test
  (t/with-instrument `maps/filter-map
    (t/is-spec-check maps/filter-map))
  (t/with-instrument :all
    (t/is= {:a 1}
      (maps/filter-map (fn [k v]
                         (and (number? v) (< v 2)))
        {:a 1 :b 3}))))

(t/deftest submap?-test
  (t/with-instrument `maps/filter-map
    (t/is-spec-check maps/filter-map))
  (t/with-instrument :all
    (t/is (maps/submap? {:a 1 :b 3} {:a 1}))
    (t/is-not (maps/submap? {:a 1 :b 3} {:a 1 :b 2}))
    (t/is-not (maps/submap? {:a 1 :b 3} {:a 2}))))

(t/deftest select-keys-by-test
  ;; skip instrumentation - function arg causes issues
  ;; select keywords only
  (t/is= {:a 1 :c 3} (maps/select-keys-by keyword? {:a 1 "b" 2 :c 3}))
  ;; none match
  (t/is= {} (maps/select-keys-by keyword? {"a" 1 "b" 2}))
  ;; all match
  (t/is= {:a 1 :b 2} (maps/select-keys-by keyword? {:a 1 :b 2}))
  ;; empty map
  (t/is= {} (maps/select-keys-by keyword? {}))
  ;; preserves sorted map type
  (t/is (maps/sorted-map? (maps/select-keys-by (fn [k] (> k 1)) (sorted-map 1 :a 2 :b 3 :c)))))

(t/deftest remove-keys-by-test
  ;; skip instrumentation - function arg causes issues
  ;; remove keywords only
  (t/is= {"b" 2} (maps/remove-keys-by keyword? {:a 1 "b" 2 :c 3}))
  ;; none match
  (t/is= {"a" 1 "b" 2} (maps/remove-keys-by keyword? {"a" 1 "b" 2}))
  ;; all match
  (t/is= {} (maps/remove-keys-by keyword? {:a 1 :b 2}))
  ;; empty map
  (t/is= {} (maps/remove-keys-by keyword? {})))

(t/deftest remove-map-test
  ;; skip spec-check - function arg causes issues
  ;; basic usage
  (t/is= {:a 5} (maps/remove-map (fn [_k v] (> v 10)) {:a 5 :b 15 :c 20}))
  ;; empty result
  (t/is= {} (maps/remove-map (fn [_k _v] true) {:a 1 :b 2}))
  ;; nothing removed
  (t/is= {:a 1 :b 2} (maps/remove-map (fn [_k _v] false) {:a 1 :b 2})))

(t/deftest invert-map-test
  (t/with-instrument `maps/invert-map
    (t/is-spec-check maps/invert-map))
  (t/with-instrument :all
    (t/is= {1 :a 2 :b 3 :c} (maps/invert-map {:a 1 :b 2 :c 3}))
    (t/is= {:a 1 :b 2} (maps/invert-map {1 :a 2 :b}))
    (t/is= {} (maps/invert-map {}))))

(t/deftest map-kv-test
  ;; skip instrumentation - function arg causes issues
  ;; transform both key and value
  (t/is= {"a" 2 "b" 3} (maps/map-kv (fn [k v] [(name k) (inc v)]) {:a 1 :b 2}))
  ;; invert using map-kv
  (t/is= {1 :a 2 :b} (maps/map-kv (fn [k v] [v k]) {:a 1 :b 2}))
  ;; empty map
  (t/is= {} (maps/map-kv (fn [k v] [v k]) {})))

(t/deftest update-some-test
  ;; skip spec-check - ifn? has no generator
  ;; key exists
  (t/is= {:a 2 :b 2} (maps/update-some {:a 1 :b 2} :a inc))
  ;; key doesn't exist - no change
  (t/is= {:a 1 :b 2} (maps/update-some {:a 1 :b 2} :c inc))
  ;; with extra args
  (t/is= {:a 31} (maps/update-some {:a 1} :a + 10 20))
  ;; empty map
  (t/is= {} (maps/update-some {} :a inc)))
