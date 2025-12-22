(ns provisdom.utility-belt.arities-test
  (:require
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.arities :as arities]))

;1 seconds

(set! *warn-on-reflection* true)

(defn hi
  ([] "")
  ([x] "")
  ([x y] "")
  ([x y & z] ""))

(ct/deftest arities-test
  (t/with-instrument `arities/arities
    (t/is-spec-check arities/arities))
  (t/with-instrument :all
    (t/is= [{::arities/parameters 0
           ::arities/variadic?  false}
          {::arities/parameters 1
           ::arities/variadic?  false}
          {::arities/parameters 2
           ::arities/variadic?  false}
          {::arities/parameters 3
           ::arities/variadic?  true}]
      (arities/arities hi))))
