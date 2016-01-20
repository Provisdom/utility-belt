(ns provisdom.test.utility-belt.core
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [pro.test :refer :all]
            [pro.core :refer :all]))

(facts "find-first"
       (fact "normally works like this"
             (find-first #(>= % 2.5) [1.0 2.0 3.0 4.0 3.0]) => 3.0)
       (fact "pre-condition is that inputs are a function and a collection"
             (find-first 1.0 1.0) => (throws))) ;put in pre-condition

(facts "sum floating-point series"
       (fact "kahan sum for precision"
             (kahan-sum [1.02 3.05 7.05]) => 11.12
             (kahan-sum [1 3 7]) => 11.0))
