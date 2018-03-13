(ns provisdom.utility-belt.nils-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.nils :as nils]
    [provisdom.utility-belt.anomalies :as anomalies]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;34 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest ignore-nils-test
  (is (spec-check nils/ignore-nils {:coll-check-limit 10
                                    :coll-error-limit 10
                                    :fspec-iterations 10
                                    :recursion-limit  1
                                    :test-check       {:num-tests 150}}))
  (is-not (nils/ignore-nils (fn [& args]
                              (when (and (= 1 (count args))
                                         (number? (first args)))
                                (< (first args) 2)))
                            nil
                            4
                            nil))
  (is (nils/ignore-nils (fn [& args]
                          (when (and (= 1 (count args))
                                     (number? (first args)))
                            (< (first args) 2)))
                        nil
                        1
                        nil))
  (is (nils/ignore-nils (fn [& args]
                          (when (and (= 1 (count args))
                                     (number? (first args)))
                            (< (first args) 2)))
                        1)))

(deftest anomaly-nils-test
  (is (spec-check nils/anomaly-nils {:coll-check-limit 10
                                     :coll-error-limit 10
                                     :fspec-iterations 10
                                     :recursion-limit  1
                                     :test-check       {:num-tests 200}}))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "nil not allowed"
        ::anomalies/fn       (var nils/anomaly-nils)}
       (nils/anomaly-nils (fn [& args]
                            (when (and (= 1 (count args))
                                       (number? (first args)))
                              (< (first args) 2)))
                          nil
                          4
                          nil))
  (is= {::anomalies/category ::anomalies/forbidden
        ::anomalies/message  "nil not allowed"
        ::anomalies/fn       (var nils/anomaly-nils)}
       (nils/anomaly-nils (fn [& args]
                            (when (and (= 1 (count args))
                                       (number? (first args)))
                              (< (first args) 2)))
                          4))
  (is (nils/anomaly-nils (fn [& args]
                           (when (and (= 1 (count args))
                                      (number? (first args)))
                             (< (first args) 2)))
                         1)))

(deftest nil-nils-test
  (is (spec-check nils/nil-nils {:coll-check-limit 10
                                 :coll-error-limit 10
                                 :fspec-iterations 10
                                 :recursion-limit  1
                                 :test-check       {:num-tests 200}}))
  (is= nil
       (nils/nil-nils (fn [& args]
                        (when (and (= 1 (count args))
                                   (number? (first args)))
                          (< (first args) 2)))
                      nil 4
                      nil))
  (is-not (nils/nil-nils (fn [& args]
                           (when (and (= 1 (count args))
                                      (number? (first args)))
                             (< (first args) 2)))
                         4)))

(deftest replace-nils-test
  (is (spec-check nils/replace-nils))
  (is= [1 2 3 4 5] (nils/replace-nils [1 2 3 nil 5] [4 nil 5]))
  (is= [1 2 4 nil 5] (nils/replace-nils [1 2 nil nil nil] [4 nil 5]))
  (is= [1 2 4 5 nil] (nils/replace-nils [1 2 nil nil nil] [4 5])))

#_(ost/unstrument)
