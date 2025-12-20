(ns provisdom.utility-belt.async-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.async :as async]))

;36 seconds

(set! *warn-on-reflection* true)

(ct/deftest catch-error-or-exception-or-nil-test
  (t/with-instrument `async/catch-error-or-exception-or-nil
    (t/is-spec-check async/catch-error-or-exception-or-nil))
  (t/with-instrument (st/instrumentable-syms)
    (t/is (async/catch-error-or-exception-or-nil (constantly true)))
    (t/is= {::anomalies/message  "HI"
          ::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var async/catch-error-or-exception-or-nil)}
      (async/catch-error-or-exception-or-nil (constantly (Exception. "HI"))))
    (t/is= {::anomalies/message  "'nil' return"
          ::anomalies/category ::anomalies/exception
          ::anomalies/fn       (var async/catch-error-or-exception-or-nil)}
      (async/catch-error-or-exception-or-nil (constantly nil)))))

(ct/deftest thread-test
  (t/with-instrument `async/thread
    (t/is-spec-check async/thread))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= [] (async/thread :and []))
    (t/is= nil
      (async/thread :and [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (t/is= [2 1] (async/thread :and [(constantly 2) (constantly 1)]))
    #_(t/is= 2
        (async/thread :first!! [(constantly 2)
                                (constantly 1)
                                (constantly {::anomalies/category
                                             ::anomalies/exception})]))
    (t/is= nil
      (async/thread :first!! [(constantly {::anomalies/category
                                           ::anomalies/exception})]))
    (t/is= nil (async/thread :first!! []))
    (t/is (async/thread :or [(constantly 2)
                           (constantly 1)
                           (constantly {::anomalies/category
                                        ::anomalies/exception})]))
    (t/is-not (async/thread :or [(constantly {::anomalies/category
                                            ::anomalies/exception})]))
    (t/is-not (async/thread :or []))
    (t/is= [2 0]
      (async/thread :any-ordered
        [(constantly 2)
         (constantly 1)
         (constantly {::anomalies/category
                      ::anomalies/exception})]))
    (t/is= [2 1]
      (async/thread :any-ordered
        [(constantly {::anomalies/category ::anomalies/exception})
         (constantly 2)
         (constantly 1)]))
    (t/is= nil
      (async/thread :any-ordered
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))
    (t/is= nil (async/thread :any-ordered []))
    (t/is= [2 1 {::anomalies/category ::anomalies/exception}]
      (async/thread :all [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (t/is= [] (async/thread :all []))))

(ct/deftest thread-select-test
  (t/with-instrument `async/thread-select
    (t/is-spec-check async/thread-select))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= [4 2]
      (async/thread-select (fn [results]
                             (when (and results
                                     (not (empty? results))
                                     (every? number? results))
                               (mapv (partial * 2) results)))
        [(constantly 2)
         (constantly 1)
         (constantly {::anomalies/category
                      ::anomalies/exception})]))
    (t/is= nil
      (async/thread-select (fn [results]
                             (when (and (not (empty? results))
                                     (every? number? results))
                               (mapv (partial * 2) results)))
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))))

(ct/deftest thread-max-test
  (t/with-instrument `async/thread-max
    (t/is-spec-check async/thread-max))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 2
      (async/thread-max [(constantly 2)
                         (constantly 1)
                         (constantly {::anomalies/category
                                      ::anomalies/exception})]))
    (t/is= 2 (async/thread-max [(constantly 2) (constantly 1)]))
    (t/is= {::anomalies/category ::anomalies/forbidden
          ::anomalies/message  "All functions 'fs' must return numbers."
          ::anomalies/fn       (var async/thread-max)}
      (async/thread-max [(constantly 2) (constantly 1) (constantly "A")]))))

(ct/deftest thread-min-test
  (t/with-instrument `async/thread-min
    (t/is-spec-check async/thread-min))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 1
      (async/thread-min [(constantly 2)
                         (constantly 1)
                         (constantly {::anomalies/category
                                      ::anomalies/exception})]))
    (t/is= 1 (async/thread-min [(constantly 2) (constantly 1)]))
    (t/is= {::anomalies/category ::anomalies/forbidden
          ::anomalies/message  "All functions 'fs' must return numbers."
          ::anomalies/fn       (var async/thread-min)}
      (async/thread-min [(constantly 2) (constantly 1) (constantly "A")]))))
