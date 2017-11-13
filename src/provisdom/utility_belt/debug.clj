(ns provisdom.utility-belt.debug
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(defmacro dbg
  "Wrap an expression with (dbg expression) to see it print out during debugging."
  [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))