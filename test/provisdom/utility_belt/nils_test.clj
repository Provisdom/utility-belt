(ns provisdom.utility-belt.nils-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.nils :as nils]))

;27 seconds

(set! *warn-on-reflection* true)

(deftest ignore-nils-test
  (t/with-instrument `nils/ignore-nils
    (t/is-spec-check nils/ignore-nils
          {:fspec-iterations 3})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (nils/ignore-nils (fn [& args]
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
  (t/with-instrument `nils/anomaly-nils
    (t/is-spec-check nils/anomaly-nils
          {:fspec-iterations 3})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= {::anomalies/category ::anomalies/forbidden
          ::anomalies/message  "nil not allowed"
          ::anomalies/fn       (var nils/anomaly-nils)}
      (nils/anomaly-nils (fn [& args]
                           (when (and (= 1 (count args))
                                   (number? (first args)))
                             (< (first args) 2)))
        nil
        4
        nil))
    (t/is= {::anomalies/category ::anomalies/forbidden
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
  (t/with-instrument `nils/nil-nils
    (t/is-spec-check nils/nil-nils
          {:fspec-iterations 3})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= nil
      (nils/nil-nils (fn [& args]
                       (when (and (= 1 (count args))
                               (number? (first args)))
                         (< (first args) 2)))
        nil 4
        nil))
    (t/is-not (nils/nil-nils (fn [& args]
                             (when (and (= 1 (count args))
                                     (number? (first args)))
                               (< (first args) 2)))
              4))))

(deftest replace-nils-test
  (t/with-instrument `nils/replace-nils
    (t/is-spec-check nils/replace-nils))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= [1 2 3 4 5] (nils/replace-nils [1 2 3 nil 5] [4 nil 5]))
    (t/is= [1 2 4 nil 5] (nils/replace-nils [1 2 nil nil nil] [4 nil 5]))
    (t/is= [1 2 4 5 nil] (nils/replace-nils [1 2 nil nil nil] [4 5]))))
