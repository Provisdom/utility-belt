(ns provisdom.utility-belt.debug-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.debug :as debug]
    [provisdom.utility-belt.anomalies :as anomalies]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest dbg-test
  (with-instrument `debug/dbg
    (is (spec-check debug/dbg)))
  (with-instrument (st/instrumentable-syms)
    (is= {::anomalies/message "Test"} (debug/dbg {::anomalies/message "Test"}))))

