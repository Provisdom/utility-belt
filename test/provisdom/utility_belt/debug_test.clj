(ns provisdom.utility-belt.debug-test
  (:require
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.debug :as debug]))

;1 seconds

(set! *warn-on-reflection* true)

(ct/deftest dbg-test
  (t/with-instrument `debug/dbg
    (t/is-spec-check debug/dbg))
  (t/with-instrument :all
    (t/is= {::anomalies/message "Test"} (debug/dbg {::anomalies/message "Test"}))))

