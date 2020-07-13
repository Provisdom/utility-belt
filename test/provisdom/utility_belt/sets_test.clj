(ns provisdom.utility-belt.sets-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.sets :as sets]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;0 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

#_(ost/unstrument)
