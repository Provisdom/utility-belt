(ns provisdom.utility-belt.t-debug
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.debug :as debug]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;0 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

#_(deftest dbg-test
  (is-not (debug/dbg {::anomalies/message "Test"})))

#_(ost/unstrument)
