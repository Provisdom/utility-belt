(ns provisdom.utility-belt.t-strings
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.strings :as strings]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest maybe-keyword-to-string-test
  (is= "hello" (strings/maybe-keyword-to-string :hello))
  (is= "hello" (strings/maybe-keyword-to-string "hello")))

(deftest substring-test
  (is= "h" (strings/substring "hi" 0))
  (is= "h" (strings/substring "hie" 1 2)))

(deftest trim-end-test
  (is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
  (is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r")))

(deftest trim-start-test
  (is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
  (is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a")))

(deftest rest-string-test
  (is= "i you" (strings/rest-string "hi you"))
  (is= "" (strings/rest-string "h"))
  (is= "" (strings/rest-string "")))

(deftest butlast-string-test
  (is= "hi yo" (strings/butlast-string "hi you"))
  (is= "" (strings/butlast-string "u"))
  (is= "" (strings/butlast-string "")))

(deftest insert-test
  (is= "hi yjumpou" (strings/insert "hi you" 4 "jump")))

(deftest abbreviate-test
  (is= "hello w..." (strings/abbreviate "hello what's up?" 10))
  (is= "hello" (strings/abbreviate "hello" 10)))

(defspec-test test-maybe-keyword-to-string `strings/maybe-keyword-to-string)
(defspec-test test-substring `strings/substring)
(defspec-test test-trim-end `strings/trim-end)
(defspec-test test-trim-start `strings/trim-start)
(defspec-test test-rest-string `strings/rest-string)
(defspec-test test-butlast-string `strings/butlast-string)
(defspec-test test-insert `strings/insert)
(defspec-test test-abbreviate `strings/abbreviate)

#_(ost/unstrument)