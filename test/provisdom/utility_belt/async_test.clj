(ns provisdom.utility-belt.async-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.async :as async]))

;36 seconds

(set! *warn-on-reflection* true)

(deftest catch-error-or-exception-or-nil-test
  (with-instrument `async/catch-error-or-exception-or-nil
    (is (spec-check async/catch-error-or-exception-or-nil)))
  (with-instrument (st/instrumentable-syms)
    (is (async/catch-error-or-exception-or-nil (constantly true)))
    (is= {::anomalies/message  "HI"
          ::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var async/catch-error-or-exception-or-nil)}
      (async/catch-error-or-exception-or-nil (constantly (Exception. "HI"))))
    (is= {::anomalies/message  "'nil' return"
          ::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var async/catch-error-or-exception-or-nil)}
      (async/catch-error-or-exception-or-nil (constantly nil)))))

(deftest thread-test
  (with-instrument `async/thread
    (is (spec-check async/thread)))
  (with-instrument (st/instrumentable-syms)
    (is= [] (async/thread :and []))
    (is= nil
      (async/thread :and [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (is= [2 1] (async/thread :and [(constantly 2) (constantly 1)]))
    #_(is= 2
        (async/thread :first!! [(constantly 2)
                                (constantly 1)
                                (constantly {::anomalies/category
                                             ::anomalies/exception})]))
    (is= nil
      (async/thread :first!! [(constantly {::anomalies/category
                                           ::anomalies/exception})]))
    (is= nil (async/thread :first!! []))
    (is (async/thread :or [(constantly 2)
                           (constantly 1)
                           (constantly {::anomalies/category
                                        ::anomalies/exception})]))
    (is-not (async/thread :or [(constantly {::anomalies/category
                                            ::anomalies/exception})]))
    (is-not (async/thread :or []))
    (is= [2 0]
      (async/thread :any-ordered
        [(constantly 2)
         (constantly 1)
         (constantly {::anomalies/category
                      ::anomalies/exception})]))
    (is= [2 1]
      (async/thread :any-ordered
        [(constantly {::anomalies/category ::anomalies/exception})
         (constantly 2)
         (constantly 1)]))
    (is= nil
      (async/thread :any-ordered
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))
    (is= nil (async/thread :any-ordered []))
    (is= [2 1 {::anomalies/category ::anomalies/exception}]
      (async/thread :all [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (is= [] (async/thread :all []))))

(deftest thread-select-test
  (with-instrument `async/thread-select
    (is (spec-check async/thread-select)))
  (with-instrument (st/instrumentable-syms)
    (is= [4 2]
      (async/thread-select (fn [results]
                             (when (and results
                                     (not (empty? results))
                                     (every? number? results))
                               (mapv (partial * 2) results)))
        [(constantly 2)
         (constantly 1)
         (constantly {::anomalies/category
                      ::anomalies/exception})]))
    (is= nil
      (async/thread-select (fn [results]
                             (when (and (not (empty? results))
                                     (every? number? results))
                               (mapv (partial * 2) results)))
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))))

(deftest thread-max-test
  (with-instrument `async/thread-max
    (is (spec-check async/thread-max)))
  (with-instrument (st/instrumentable-syms)
    (is= 2
      (async/thread-max [(constantly 2)
                         (constantly 1)
                         (constantly {::anomalies/category
                                      ::anomalies/exception})]))
    (is= 2 (async/thread-max [(constantly 2) (constantly 1)]))
    (is= {::anomalies/category ::anomalies/forbidden
          ::anomalies/message  "All functions 'fs' must return numbers."
          ::anomalies/fn       (var async/thread-max)}
      (async/thread-max [(constantly 2) (constantly 1) (constantly "A")]))))

(deftest thread-min-test
  (with-instrument `async/thread-min
    (is (spec-check async/thread-min)))
  (with-instrument (st/instrumentable-syms)
    (is= 1
      (async/thread-min [(constantly 2)
                         (constantly 1)
                         (constantly {::anomalies/category
                                      ::anomalies/exception})]))
    (is= 1 (async/thread-min [(constantly 2) (constantly 1)]))
    (is= {::anomalies/category ::anomalies/forbidden
          ::anomalies/message  "All functions 'fs' must return numbers."
          ::anomalies/fn       (var async/thread-min)}
      (async/thread-min [(constantly 2) (constantly 1) (constantly "A")]))))
