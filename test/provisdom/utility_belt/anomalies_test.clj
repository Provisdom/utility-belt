(ns provisdom.utility-belt.anomalies-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.anomalies :as anomalies]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;1 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest anomaly?-test
  (is (spec-check anomalies/anomaly?))
  (is (anomalies/anomaly? {::anomalies/category ::anomalies/no-solve}))
  (is-not (anomalies/anomaly? {::anomalies/message "Test"})))

#_(ost/unstrument)
