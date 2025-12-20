(ns provisdom.utility-belt.sorted-sets-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.sorted-sets :as sorted-sets]))

;1 seconds

(set! *warn-on-reflection* true)

(ct/deftest sorted-set?-test
  (t/with-instrument `sorted-sets/sorted-set?
    (t/is-spec-check sorted-sets/sorted-set?))
  (t/with-instrument (st/instrumentable-syms)
    (t/is-not (sorted-sets/sorted-set? [:a :b]))
    (t/is-not (sorted-sets/sorted-set? #{:a :b}))
    (t/is (sorted-sets/sorted-set? (sorted-set :a :b)))))

(ct/deftest sorted-set-by?-test
  (t/is (sorted-sets/sorted-set-by? > (sorted-set-by > 1 2)))
  (t/is-not (sorted-sets/sorted-set-by? > #{1 2}))
  (t/is (sorted-sets/sorted-set-by? < (sorted-set 1 2))))
