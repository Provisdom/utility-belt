(ns provisdom.utility-belt.arities-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.arities :as arities]))

;1 seconds

(set! *warn-on-reflection* true)

(defn hi
  ([] "")
  ([x] "")
  ([x y] "")
  ([x y & z] ""))

(deftest arities-test
  (with-instrument `arities/arities
    (is (spec-check arities/arities)))
  (with-instrument (st/instrumentable-syms)
  (is= [{::arities/parameters 0
         ::arities/variadic? false}
        {::arities/parameters 1
         ::arities/variadic? false}
        {::arities/parameters 2
         ::arities/variadic? false}
        {::arities/parameters 3
         ::arities/variadic? true}]
       (arities/arities hi))))
