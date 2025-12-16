(ns provisdom.utility-belt.sorted-sets-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.sorted-sets :as sorted-sets]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest sorted-set?-test
  (t/with-instrument `sorted-sets/sorted-set?
    (is (t/spec-check sorted-sets/sorted-set?)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (sorted-sets/sorted-set? [:a :b]))
    (t/is-not (sorted-sets/sorted-set? #{:a :b}))
    (is (sorted-sets/sorted-set? (sorted-set :a :b)))))

(deftest sorted-set-by?-test
  (is (sorted-sets/sorted-set-by? > (sorted-set-by > 1 2)))
  (t/is-not (sorted-sets/sorted-set-by? > #{1 2}))
  (is (sorted-sets/sorted-set-by? < (sorted-set 1 2))))
