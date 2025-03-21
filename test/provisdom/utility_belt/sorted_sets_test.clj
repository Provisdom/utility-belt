(ns provisdom.utility-belt.sorted-sets-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.sorted-sets :as sorted-sets]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest sorted-set?-test
  (with-instrument `sorted-sets/sorted-set?
    (is (spec-check sorted-sets/sorted-set?)))
  (with-instrument (st/instrumentable-syms)
    (is-not (sorted-sets/sorted-set? [:a :b]))
    (is-not (sorted-sets/sorted-set? #{:a :b}))
    (is (sorted-sets/sorted-set? (sorted-set :a :b)))))

(deftest sorted-set-by?-test
  (is (sorted-sets/sorted-set-by? > (sorted-set-by > 1 2)))
  (is-not (sorted-sets/sorted-set-by? > #{1 2}))
  (is (sorted-sets/sorted-set-by? < (sorted-set 1 2))))
