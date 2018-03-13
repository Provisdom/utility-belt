(ns provisdom.utility-belt.async-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.async :as async]
    [provisdom.utility-belt.anomalies :as anomalies]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;8 seconds -- problems with gen-testing in this ns

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest catch-error-or-exception-test
  (is (spec-check async/catch-error-or-exception))
  (is (async/catch-error-or-exception (constantly true)))
  (is= {::anomalies/message  "HI"
        ::anomalies/category ::anomalies/exception
        ::anomalies/fn       (var async/catch-error-or-exception)}
       (async/catch-error-or-exception (constantly (Exception. "HI"))))
  (is= nil (async/catch-error-or-exception (constantly nil))))

(deftest thread-test
  #_(is (spec-check async/thread {:coll-check-limit 10
                                  :coll-error-limit 10
                                  :fspec-iterations 10
                                  :recursion-limit  1
                                  :test-check       {:num-tests 1}}))
  (is= [] (async/thread :and []))
  (is= nil
       (async/thread :and [(constantly 2)
                           (constantly 1)
                           (constantly {::anomalies/category ::anomalies/exception})]))
  (is= [2 1] (async/thread :and [(constantly 2) (constantly 1)]))
  #_(is= 2
         (async/thread :first!! [(constantly 2)
                                 (constantly 1)
                                 (constantly {::anomalies/category ::anomalies/exception})]))
  (is= nil
       (async/thread :first!! [(constantly {::anomalies/category ::anomalies/exception})]))
  (is= nil (async/thread :first!! []))
  (is (async/thread :or [(constantly 2)
                         (constantly 1)
                         (constantly {::anomalies/category ::anomalies/exception})]))
  (is-not (async/thread :or [(constantly {::anomalies/category ::anomalies/exception})]))
  (is-not (async/thread :or []))
  (is= [2 0]
       (async/thread :any-ordered
                     [(constantly 2)
                      (constantly 1)
                      (constantly {::anomalies/category ::anomalies/exception})]))
  (is= [2 1]
       (async/thread :any-ordered
                     [(constantly {::anomalies/category ::anomalies/exception})
                      (constantly 2)
                      (constantly 1)]))
  (is= nil
       (async/thread :any-ordered
                     [(constantly {::anomalies/category ::anomalies/exception})]))
  (is= nil (async/thread :any-ordered []))
  (is= [2 1 {::anomalies/category ::anomalies/exception}]
       (async/thread :all [(constantly 2)
                           (constantly 1)
                           (constantly {::anomalies/category ::anomalies/exception})]))
  (is= [] (async/thread :all [])))

(deftest thread-select-test
  #_(is (spec-check async/thread-select {:coll-check-limit 10
                                         :coll-error-limit 10
                                         :fspec-iterations 10
                                         :recursion-limit  1
                                         :test-check       {:num-tests 1}}))
  (is= [4 2]
       (async/thread-select (fn [results]
                              (when (and results
                                         (not (empty? results))
                                         (every? number? results))
                                (mapv (partial * 2) results)))
                            [(constantly 2)
                             (constantly 1)
                             (constantly {::anomalies/category ::anomalies/exception})]))
  (is= nil
       (async/thread-select (fn [results]
                              (when (and (not (empty? results))
                                         (every? number? results))
                                (mapv (partial * 2) results)))
                            [(constantly {::anomalies/category ::anomalies/exception})])))

(deftest thread-max-test
  #_(is (spec-check async/thread-max {:coll-check-limit 10
                                      :coll-error-limit 10
                                      :fspec-iterations 10
                                      :recursion-limit  1
                                      :test-check       {:num-tests 1}}))
  (is= 2
       (async/thread-max [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category ::anomalies/exception})]))
  (is= 2 (async/thread-max [(constantly 2) (constantly 1)]))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "All functions 'fs' must return numbers."
        ::anomalies/fn       (var async/thread-max)}
       (async/thread-max [(constantly 2) (constantly 1) (constantly "A")])))

(deftest thread-min-test
  #_(is (spec-check async/thread-min {:coll-check-limit 10
                                      :coll-error-limit 10
                                      :fspec-iterations 10
                                      :recursion-limit  1
                                      :test-check       {:num-tests 1}}))
  (is= 1
       (async/thread-min [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category ::anomalies/exception})]))
  (is= 1 (async/thread-min [(constantly 2) (constantly 1)]))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "All functions 'fs' must return numbers."
        ::anomalies/fn       (var async/thread-min)}
       (async/thread-min [(constantly 2) (constantly 1) (constantly "A")])))

#_(ost/unstrument)