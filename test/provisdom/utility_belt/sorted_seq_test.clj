(ns provisdom.utility-belt.sorted-seq-test
  (:refer-clojure :exclude [sorted?])
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.sorted-seq :as sorted-seq]))

;;27 seconds

;; ## Testing Notes for sorted-seq
;;
;; Functions in this namespace have two categories with different spec-check/instrumentation
;; behavior:
;;
;; **Non-`-by` functions** (`sorted?`, `strictly-sorted?`, etc.): These use Clojure's `compare`
;; internally, which throws on mixed types (e.g., comparing a number to a string). The specs have
;; custom generators producing homogeneous collections, so spec-check works. Instrumentation also
;; works normally.
;;
;; **`-by` functions** (`sorted-by?`, `strictly-sorted-by?`, etc.): These accept a comparator
;; function argument. Orchestra validates `s/fspec` args by calling them with generated values,
;; which fails for comparators like `compare` that don't handle arbitrary types. Spec-check works
;; (uses the generator's sample function), but instrumentation must be skipped for these functions.

(set! *warn-on-reflection* true)

;;;GENERIC SORTED PREDICATES
(t/deftest sorted?-test
  (t/with-instrument `sorted-seq/sorted?
    (t/is-spec-check sorted-seq/sorted?))
  (t/with-instrument :all
    (t/is (sorted-seq/sorted? []))
    (t/is (sorted-seq/sorted? [1]))
    (t/is (sorted-seq/sorted? [1 2 3]))
    (t/is (sorted-seq/sorted? [1 1 2 3]))
    (t/is-not (sorted-seq/sorted? [3 2 1]))
    (t/is (sorted-seq/sorted? '(1 2 3)))
    (t/is (sorted-seq/sorted? [:a :b :c]))))

(t/deftest sorted-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/sorted-by? {:num-tests 300})
  (t/is (sorted-seq/sorted-by? compare [1 2 3]))
  (t/is (sorted-seq/sorted-by? #(- (compare %1 %2)) [3 2 1]))
  (t/is-not (sorted-seq/sorted-by? compare [3 2 1])))

(t/deftest strictly-sorted?-test
  (t/with-instrument `sorted-seq/strictly-sorted?
    (t/is-spec-check sorted-seq/strictly-sorted?))
  (t/with-instrument :all
    (t/is (sorted-seq/strictly-sorted? []))
    (t/is (sorted-seq/strictly-sorted? [1]))
    (t/is (sorted-seq/strictly-sorted? [1 2 3]))
    (t/is-not (sorted-seq/strictly-sorted? [1 1 2 3]))
    (t/is-not (sorted-seq/strictly-sorted? [3 2 1]))))

(t/deftest strictly-sorted-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/strictly-sorted-by?)
  (t/is (sorted-seq/strictly-sorted-by? compare [1 2 3]))
  (t/is-not (sorted-seq/strictly-sorted-by? compare [1 1 2]))
  (t/is (sorted-seq/strictly-sorted-by? #(- (compare %1 %2)) [3 2 1])))

(t/deftest sorted-desc?-test
  (t/with-instrument `sorted-seq/sorted-desc?
    (t/is-spec-check sorted-seq/sorted-desc?))
  (t/with-instrument :all
    (t/is (sorted-seq/sorted-desc? []))
    (t/is (sorted-seq/sorted-desc? [1]))
    (t/is (sorted-seq/sorted-desc? [3 2 1]))
    (t/is (sorted-seq/sorted-desc? [3 3 2 1]))
    (t/is-not (sorted-seq/sorted-desc? [1 2 3]))))

(t/deftest sorted-desc-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/sorted-desc-by?)
  (t/is (sorted-seq/sorted-desc-by? compare [3 2 1]))
  (t/is-not (sorted-seq/sorted-desc-by? compare [1 2 3])))

(t/deftest strictly-sorted-desc?-test
  (t/with-instrument `sorted-seq/strictly-sorted-desc?
    (t/is-spec-check sorted-seq/strictly-sorted-desc?))
  (t/with-instrument :all
    (t/is (sorted-seq/strictly-sorted-desc? []))
    (t/is (sorted-seq/strictly-sorted-desc? [1]))
    (t/is (sorted-seq/strictly-sorted-desc? [3 2 1]))
    (t/is-not (sorted-seq/strictly-sorted-desc? [3 3 2 1]))
    (t/is-not (sorted-seq/strictly-sorted-desc? [1 2 3]))))

(t/deftest strictly-sorted-desc-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/strictly-sorted-desc-by?)
  (t/is (sorted-seq/strictly-sorted-desc-by? compare [3 2 1]))
  (t/is-not (sorted-seq/strictly-sorted-desc-by? compare [3 3 2 1])))

;;;SEQ -- SORTED
(t/deftest seq-sorted?-test
  (t/with-instrument `sorted-seq/seq-sorted?
    (t/is-spec-check sorted-seq/seq-sorted?))
  (t/with-instrument :all
    (t/is (sorted-seq/seq-sorted? [1 2 3]))
    (t/is (sorted-seq/seq-sorted? '(1 2 3)))
    (t/is-not (sorted-seq/seq-sorted? #{1 2 3}))
    (t/is-not (sorted-seq/seq-sorted? [3 2 1]))))

(t/deftest seq-sorted-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/seq-sorted-by?)
  (t/is (sorted-seq/seq-sorted-by? compare [1 2 3]))
  (t/is (sorted-seq/seq-sorted-by? compare '(1 2 3)))
  (t/is-not (sorted-seq/seq-sorted-by? compare #{1 2 3})))

;;;LIST -- SORTED
(t/deftest list-sorted?-test
  (t/with-instrument `sorted-seq/list-sorted?
    (t/is-spec-check sorted-seq/list-sorted?))
  (t/with-instrument :all
    (t/is-not (sorted-seq/list-sorted? [:a :b]))
    (t/is (sorted-seq/list-sorted? '(:a :b)))
    (t/is-not (sorted-seq/list-sorted? '(:b :a)))))

(t/deftest list-sorted-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/list-sorted-by?)
  (t/is-not (sorted-seq/list-sorted-by? #(- (compare %1 %2)) [1 2]))
  (t/is-not (sorted-seq/list-sorted-by? #(- (compare %1 %2)) '(1 2)))
  (t/is (sorted-seq/list-sorted-by? compare '(1 2))))

;;;VECTOR -- SORTED
(t/deftest vector-sorted?-test
  (t/with-instrument `sorted-seq/vector-sorted?
    (t/is-spec-check sorted-seq/vector-sorted?))
  (t/with-instrument :all
    (t/is (sorted-seq/vector-sorted? [:a :b]))
    (t/is-not (sorted-seq/vector-sorted? '(:a :b)))
    (t/is-not (sorted-seq/vector-sorted? [:b :a]))))

(t/deftest vector-sorted-by?-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/vector-sorted-by?)
  (t/is-not (sorted-seq/vector-sorted-by? #(- (compare %1 %2)) [1 2]))
  (t/is-not (sorted-seq/vector-sorted-by? #(- (compare %1 %2)) '(1 2)))
  (t/is (sorted-seq/vector-sorted-by? compare [1 2])))

;;;BINARY SEARCH
(t/deftest binary-search-test
  (t/with-instrument `sorted-seq/binary-search
    (t/is-spec-check sorted-seq/binary-search))
  (t/with-instrument :all
    (t/is= nil (sorted-seq/binary-search [] 1))
    (t/is= 0 (sorted-seq/binary-search [1] 1))
    (t/is= nil (sorted-seq/binary-search [1] 2))
    (t/is= 0 (sorted-seq/binary-search [1 2 3 4 5] 1))
    (t/is= 2 (sorted-seq/binary-search [1 2 3 4 5] 3))
    (t/is= 4 (sorted-seq/binary-search [1 2 3 4 5] 5))
    (t/is= nil (sorted-seq/binary-search [1 2 3 4 5] 6))
    (t/is= nil (sorted-seq/binary-search [1 2 3 4 5] 0))
    ;; with duplicates, finds one of them
    (t/is (#{1 2} (sorted-seq/binary-search [1 2 2 2 3] 2)))))

(t/deftest binary-search-by-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/binary-search-by)
  (t/is= 2 (sorted-seq/binary-search-by compare [1 2 3 4 5] 3))
  (t/is= 2 (sorted-seq/binary-search-by #(- (compare %1 %2)) [5 4 3 2 1] 3))
  (t/is= nil (sorted-seq/binary-search-by compare [1 2 3] 5)))

(t/deftest binary-search-insertion-point-test
  (t/with-instrument `sorted-seq/binary-search-insertion-point
    (t/is-spec-check sorted-seq/binary-search-insertion-point))
  (t/with-instrument :all
    (t/is= 0 (sorted-seq/binary-search-insertion-point [] 1))
    (t/is= 0 (sorted-seq/binary-search-insertion-point [2 3 4] 1))
    (t/is= 1 (sorted-seq/binary-search-insertion-point [1 3 4] 2))
    (t/is= 3 (sorted-seq/binary-search-insertion-point [1 2 3] 4))
    (t/is= 0 (sorted-seq/binary-search-insertion-point [1 2 3] 1))
    (t/is= 1 (sorted-seq/binary-search-insertion-point [1 2 2 2 3] 2))))

(t/deftest binary-search-insertion-point-by-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/binary-search-insertion-point-by)
  (t/is= 1 (sorted-seq/binary-search-insertion-point-by compare [1 3 4] 2))
  ;; descending: 2 goes between 3 and 1, so index 2
  (t/is= 2 (sorted-seq/binary-search-insertion-point-by #(- (compare %1 %2)) [4 3 1] 2)))

;;;SORTED UTILITIES
(t/deftest insert-sorted-test
  (t/with-instrument `sorted-seq/insert-sorted
    (t/is-spec-check sorted-seq/insert-sorted))
  (t/with-instrument :all
    (t/is= [1] (sorted-seq/insert-sorted [] 1))
    (t/is= [1 2] (sorted-seq/insert-sorted [2] 1))
    (t/is= [1 2] (sorted-seq/insert-sorted [1] 2))
    (t/is= [1 2 3] (sorted-seq/insert-sorted [1 3] 2))
    (t/is= [1 2 2 3] (sorted-seq/insert-sorted [1 2 3] 2))
    (t/is= [0 1 2 3] (sorted-seq/insert-sorted [1 2 3] 0))
    (t/is= [1 2 3 4] (sorted-seq/insert-sorted [1 2 3] 4))))

(t/deftest insert-sorted-by-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/insert-sorted-by)
  (t/is= [3 2 1] (sorted-seq/insert-sorted-by #(- (compare %1 %2)) [3 1] 2))
  (t/is= [4 3 2 1] (sorted-seq/insert-sorted-by #(- (compare %1 %2)) [3 2 1] 4)))

(t/deftest merge-sorted-test
  (t/with-instrument `sorted-seq/merge-sorted
    (t/is-spec-check sorted-seq/merge-sorted))
  (t/with-instrument :all
    (t/is= [] (sorted-seq/merge-sorted [] []))
    (t/is= [1 2 3] (sorted-seq/merge-sorted [1 2 3] []))
    (t/is= [1 2 3] (sorted-seq/merge-sorted [] [1 2 3]))
    (t/is= [1 2 3 4 5 6] (sorted-seq/merge-sorted [1 3 5] [2 4 6]))
    (t/is= [1 2 2 3 4 5] (sorted-seq/merge-sorted [1 2 3] [2 4 5]))
    (t/is= [1 1 2 2 3 3] (sorted-seq/merge-sorted [1 2 3] [1 2 3]))))

(t/deftest merge-sorted-by-test
  ;;no instrumentation; Orchestra fspec validation fails with comparator args
  (t/is-spec-check sorted-seq/merge-sorted-by)
  (t/is= [6 5 4 3 2 1] (sorted-seq/merge-sorted-by #(- (compare %1 %2)) [5 3 1] [6 4 2])))
