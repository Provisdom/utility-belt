(ns provisdom.utility-belt.arities-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.arities :as arities]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;1 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(defn hi
  ([] "")
  ([x] "")
  ([x y] "")
  ([x y & z] ""))

(deftest arities-test
  (is (spec-check arities/arities))
  (is= [{::arities/parameters 0
         ::arities/variadic? false}
        {::arities/parameters 1
         ::arities/variadic? false}
        {::arities/parameters 2
         ::arities/variadic? false}
        {::arities/parameters 3
         ::arities/variadic? true}]
       (arities/arities hi)))

#_(ost/unstrument)
