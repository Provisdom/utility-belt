(ns provisdom.utility-belt.strings-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.strings :as strings]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;1 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest maybe-keyword-to-string-test
  (is (spec-check strings/maybe-keyword-to-string))
  (is= "hello" (strings/maybe-keyword-to-string :hello))
  (is= "hello" (strings/maybe-keyword-to-string "hello")))

(deftest substring-test
  (is (spec-check strings/substring))
  (is= "hi" (strings/substring "hi" 0))
  (is= "ie" (strings/substring "hie" 1 2)))

(deftest trim-end-test
  (is (spec-check strings/trim-end))
  (is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
  (is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r")))

(deftest trim-start-test
  (is (spec-check strings/trim-start))
  (is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
  (is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a")))

(deftest rest-string-test
  (is (spec-check strings/rest-string))
  (is= "i you" (strings/rest-string "hi you"))
  (is= "" (strings/rest-string "h"))
  (is= "" (strings/rest-string "")))

(deftest butlast-string-test
  (is (spec-check strings/butlast-string))
  (is= "hi yo" (strings/butlast-string "hi you"))
  (is= "" (strings/butlast-string "u"))
  (is= "" (strings/butlast-string "")))

(deftest insert-test
  (is (spec-check strings/insert))
  (is= "hi yjumpou" (strings/insert "hi you" 4 "jump")))

(deftest abbreviate-test
  (is (spec-check strings/abbreviate))
  (is= "hello w..." (strings/abbreviate "hello what's up?" 10))
  (is= "hello" (strings/abbreviate "hello" 10)))

#_(ost/unstrument)