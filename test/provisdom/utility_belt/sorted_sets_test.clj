(ns provisdom.utility-belt.sorted-sets-test
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.sorted-sets :as sorted-sets]))

;;3 seconds

;; ## Testing Notes for sorted-sets
;;
;; `sorted-set-by?` accepts a comparator function argument. Orchestra validates `s/fspec` args by
;; calling them with generated values, which fails for comparators like `compare` that don't handle
;; arbitrary types. Spec-check works (uses the generator's sample function), but instrumentation
;; must be skipped for this function.

(set! *warn-on-reflection* true)

(t/deftest sorted-set?-test
  (t/with-instrument `sorted-sets/sorted-set?
    (t/is-spec-check sorted-sets/sorted-set?))
  (t/with-instrument :all
    (t/is-not (sorted-sets/sorted-set? [:a :b]))
    (t/is-not (sorted-sets/sorted-set? #{:a :b}))
    (t/is (sorted-sets/sorted-set? (sorted-set :a :b)))))

(t/deftest sorted-set-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-sets/sorted-set-by?)
  (t/is (sorted-sets/sorted-set-by? > (sorted-set-by > 1 2)))
  (t/is-not (sorted-sets/sorted-set-by? > #{1 2}))
  (t/is (sorted-sets/sorted-set-by? < (sorted-set 1 2)))
  ;; test with compare (returns int)
  (t/is (sorted-sets/sorted-set-by? compare (sorted-set 1 2 3))))

;;;NAVIGABLE SET OPERATIONS
(t/deftest floor-test
  (t/with-instrument `sorted-sets/floor
    (t/is-spec-check sorted-sets/floor))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= 5 (sorted-sets/floor ss 5))
      (t/is= 5 (sorted-sets/floor ss 6))
      (t/is= 9 (sorted-sets/floor ss 10))
      (t/is= nil (sorted-sets/floor ss 0))
      (t/is= 1 (sorted-sets/floor ss 1)))))

(t/deftest ceiling-test
  (t/with-instrument `sorted-sets/ceiling
    (t/is-spec-check sorted-sets/ceiling))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= 5 (sorted-sets/ceiling ss 5))
      (t/is= 5 (sorted-sets/ceiling ss 4))
      (t/is= 1 (sorted-sets/ceiling ss 0))
      (t/is= nil (sorted-sets/ceiling ss 10))
      (t/is= 9 (sorted-sets/ceiling ss 9)))))

(t/deftest lower-test
  (t/with-instrument `sorted-sets/lower
    (t/is-spec-check sorted-sets/lower))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= 3 (sorted-sets/lower ss 5))
      (t/is= 5 (sorted-sets/lower ss 6))
      (t/is= 9 (sorted-sets/lower ss 10))
      (t/is= nil (sorted-sets/lower ss 1))
      (t/is= nil (sorted-sets/lower ss 0)))))

(t/deftest higher-test
  (t/with-instrument `sorted-sets/higher
    (t/is-spec-check sorted-sets/higher))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= 7 (sorted-sets/higher ss 5))
      (t/is= 7 (sorted-sets/higher ss 6))
      (t/is= 1 (sorted-sets/higher ss 0))
      (t/is= nil (sorted-sets/higher ss 9))
      (t/is= nil (sorted-sets/higher ss 10)))))

(t/deftest subset-test
  (t/with-instrument `sorted-sets/subset
    (t/is-spec-check sorted-sets/subset))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= [3 5] (vec (sorted-sets/subset ss 3 7)))
      (t/is= [3 5 7] (vec (sorted-sets/subset ss 2 8)))
      (t/is= [] (vec (sorted-sets/subset ss 10 20)))
      (t/is= [1 3 5 7 9] (vec (sorted-sets/subset ss 0 10))))))

(t/deftest subset-inclusive-test
  (t/with-instrument `sorted-sets/subset-inclusive
    (t/is-spec-check sorted-sets/subset-inclusive))
  (t/with-instrument :all
    (let [ss (sorted-set 1 3 5 7 9)]
      (t/is= [3 5 7] (vec (sorted-sets/subset-inclusive ss 3 7)))
      (t/is= [3 5 7] (vec (sorted-sets/subset-inclusive ss 2 8)))
      (t/is= [] (vec (sorted-sets/subset-inclusive ss 10 20)))
      (t/is= [1 3 5 7 9] (vec (sorted-sets/subset-inclusive ss 0 10))))))

;;;MACRO TESTS
(s/def ::test-sorted-set (sorted-sets/sorted-set-of int? :min-count 1 :gen-max 5))

(t/deftest sorted-set-of-test
  (t/with-instrument :all
    (t/is (s/valid? ::test-sorted-set (sorted-set 1 2 3)))
    (t/is-not (s/valid? ::test-sorted-set #{1 2 3}))
    (t/is-not (s/valid? ::test-sorted-set (sorted-set)))
    (t/is-not (s/valid? ::test-sorted-set (sorted-set "a" "b")))
    ;; test generator produces valid values
    (let [generated (first (s/exercise ::test-sorted-set 1))]
      (t/is (s/valid? ::test-sorted-set (first generated))))))

(s/def ::test-sorted-set-by
  (sorted-sets/sorted-set-by-of int? > :min-count 1 :gen-max 5))

(t/deftest sorted-set-by-of-test
  (t/with-instrument :all
    (t/is (s/valid? ::test-sorted-set-by (sorted-set-by > 1 2 3)))
    (t/is-not (s/valid? ::test-sorted-set-by #{1 2 3}))
    (t/is-not (s/valid? ::test-sorted-set-by (sorted-set 1 2 3)))
    ;; test generator produces valid values
    (let [generated (first (s/exercise ::test-sorted-set-by 1))]
      (t/is (s/valid? ::test-sorted-set-by (first generated))))))
