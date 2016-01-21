(ns provisdom.test.utility-belt.core
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.utility-belt.core :refer :all]))

(facts "sum floating-point series"
       (fact "kahan sum for precision"
             (kahan-sum [1.02 3.05 7.05]) => 11.12
             (kahan-sum [1 3 7]) => 11.0))
