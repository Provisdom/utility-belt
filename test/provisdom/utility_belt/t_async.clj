(ns provisdom.utility-belt.t-async
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.async :as async]
    [provisdom.utility-belt.anomalies :as anomalies]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest catch-error-or-nil-test
  (is= true (async/catch-error-or-nil (constantly true)))
  (is= false (async/catch-error-or-nil (constantly (Exception. "HI"))))
  (is= false (async/catch-error-or-nil (constantly nil)))
  (is= "barfed"
       (async/catch-error-or-nil (constantly (Exception. "HI")) "barfed"))
  (is= "nil result"
       (async/catch-error-or-nil (constantly (Exception. "HI"))
                                 (fn [e]
                                   (.getMessage ^Exception e)))))

(deftest thread-test
  (is= false (async/thread :and [(constantly 2) (constantly 1) (constantly nil)]))
  ;(is= 2 (async/thread :first!! [(constantly 2) (constantly 1) (constantly false)]))
  (is= true (async/thread :or [(constantly 2) (constantly 1) (constantly false)]))
  (is= [2 0] (async/thread :any-ordered [(constantly 2) (constantly 1) (constantly false)]))
  (is= [2 1] (async/thread :any-ordered [(constantly false) (constantly 2) (constantly 1)]))
  (is= [2 1 false] (async/thread :all [(constantly 2) (constantly 1) (constantly false)])))

(deftest thread-select-test
  (is= '(4 2)
       (async/thread-select (fn [res]
                              (map (partial * 2) res))
                            [(constantly 2) (constantly 1) (constantly false)])))

(deftest thread-max-test
  (is= 2 (async/thread-max [(constantly 2) (constantly 1) (constantly false)]))
  (is= 2 (async/thread-max [(constantly 2) (constantly 1)]))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "all functions fs must return numbers"
        ::anomalies/fn       (var async/thread-max)}
       (async/thread-max [(constantly 2) (constantly 1) (constantly "A")])))

(deftest thread-min-test
  (is= 1 (async/thread-min [(constantly 2) (constantly 1) (constantly false)]))
  (is= 1 (async/thread-min [(constantly 2) (constantly 1)]))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "all functions fs must return numbers"
        ::anomalies/fn       (var async/thread-min)}
       (async/thread-min [(constantly 2) (constantly 1) (constantly "A")])))

;(defspec-test test-catch-error-or-nil `async/catch-error-or-nil)
;(defspec-test test-thread `async/thread)
;(defspec-test test-thread-select `async/thread-select)
;(defspec-test test-thread-max `async/thread-max)
;(defspec-test test-thread-min `async/thread-min)

#_(ost/unstrument)