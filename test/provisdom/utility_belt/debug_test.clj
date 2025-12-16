(ns provisdom.utility-belt.debug-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.debug :as debug]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest dbg-test
  (t/with-instrument `debug/dbg
    (is (t/spec-check debug/dbg)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= {::anomalies/message "Test"} (debug/dbg {::anomalies/message "Test"}))))

