(ns provisdom.utility-belt.t-anomalies
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.anomalies :as anomalies]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest anomaly?-test
  (is (anomalies/anomaly? {::anomalies/category ::anomalies/no-solve}))
  (is-not (anomalies/anomaly? {::anomalies/message "Test"})))

(defspec-test test-anomaly? `anomalies/anomaly?)

#_(ost/unstrument)
