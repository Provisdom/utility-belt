(ns provisdom.utility-belt.nils-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.nils :as nils]))

;27 seconds

(set! *warn-on-reflection* true)

(deftest ignore-nils-test
  (with-instrument `nils/ignore-nils
    (is (spec-check nils/ignore-nils
          {:fspec-iterations 3})))
  (with-instrument (st/instrumentable-syms)
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
          1))))

(deftest anomaly-nils-test
  (with-instrument `nils/anomaly-nils
    (is (spec-check nils/anomaly-nils
          {:fspec-iterations 3})))
  (with-instrument (st/instrumentable-syms)
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
          1))))

(deftest nil-nils-test
  (with-instrument `nils/nil-nils
    (is (spec-check nils/nil-nils
          {:fspec-iterations 3})))
  (with-instrument (st/instrumentable-syms)
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
              4))))

(deftest replace-nils-test
  (with-instrument `nils/replace-nils
    (is (spec-check nils/replace-nils)))
  (with-instrument (st/instrumentable-syms)
    (is= [1 2 3 4 5] (nils/replace-nils [1 2 3 nil 5] [4 nil 5]))
    (is= [1 2 4 nil 5] (nils/replace-nils [1 2 nil nil nil] [4 nil 5]))
    (is= [1 2 4 5 nil] (nils/replace-nils [1 2 nil nil nil] [4 5]))))
