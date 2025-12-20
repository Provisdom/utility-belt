(ns provisdom.utility-belt.anomalies-test
  #?(:cljs (:require-macros
             [provisdom.test.core]))
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :as ct]
    [provisdom.test.core :refer [is-not spec-check with-instrument]]
    [provisdom.utility-belt.anomalies :as anomalies]
    #?(:cljs [orchestra-cljs.spec.test :as ost])))

;1 seconds

#?(:clj (set! *warn-on-reflection* true))

(ct/deftest anomaly?-test
  (with-instrument `anomalies/anomaly?
    (ct/is (spec-check anomalies/anomaly?)))
  (with-instrument (st/instrumentable-syms)
    (ct/is (anomalies/anomaly? {::anomalies/category ::anomalies/no-solve}))
    (is-not (anomalies/anomaly? {::anomalies/message "Test"}))))
