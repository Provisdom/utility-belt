(ns provisdom.utility-belt.anomalies-test
  #?(:cljs (:require-macros
             [provisdom.test.core]))
  (:require
    [clojure.test :refer [deftest is are testing]]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [provisdom.test.core :refer [is-not] :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    #?(:clj  [orchestra.spec.test :as ost]
       :cljs [orchestra-cljs.spec.test :as ost-spec])))

;1 seconds

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (ost/instrument))
#?(:cljs (ost-spec/instrument))

(deftest anomaly?-test
  (is (spec-check anomalies/anomaly?))
  (is (anomalies/anomaly? {::anomalies/category ::anomalies/no-solve}))
  (is-not (anomalies/anomaly? {::anomalies/message "Test"})))

#_(ost/unstrument)
