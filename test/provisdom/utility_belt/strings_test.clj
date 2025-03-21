(ns provisdom.utility-belt.strings-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.strings :as strings]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest maybe-keyword-to-string-test
  (with-instrument `strings/maybe-keyword-to-string
    (is (spec-check strings/maybe-keyword-to-string)))
  (with-instrument (st/instrumentable-syms)
    (is= "hello" (strings/maybe-keyword-to-string :hello))
    (is= "hello" (strings/maybe-keyword-to-string "hello"))))

(deftest substring-test
  (with-instrument `strings/substring
    (is (spec-check strings/substring)))
  (with-instrument (st/instrumentable-syms)
    (is= "hi" (strings/substring "hi" 0))
    (is= "ie" (strings/substring "hie" 1 2))))

(deftest trim-end-test
  (with-instrument `strings/trim-end
    (is (spec-check strings/trim-end)))
  (with-instrument (st/instrumentable-syms)
    (is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
    (is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r"))))

(deftest trim-start-test
  (with-instrument `strings/trim-start
    (is (spec-check strings/trim-start)))
  (with-instrument (st/instrumentable-syms)
    (is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
    (is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a"))))

(deftest rest-string-test
  (with-instrument `strings/rest-string
    (is (spec-check strings/rest-string)))
  (with-instrument (st/instrumentable-syms)
    (is= "i you" (strings/rest-string "hi you"))
    (is= "" (strings/rest-string "h"))
    (is= "" (strings/rest-string ""))))

(deftest butlast-string-test
  (with-instrument `strings/butlast-string
    (is (spec-check strings/butlast-string)))
  (with-instrument (st/instrumentable-syms)
    (is= "hi yo" (strings/butlast-string "hi you"))
    (is= "" (strings/butlast-string "u"))
    (is= "" (strings/butlast-string ""))))

(deftest insert-test
  (with-instrument `strings/insert
    (is (spec-check strings/insert)))
  (with-instrument (st/instrumentable-syms)
    (is= "hi yjumpou" (strings/insert "hi you" 4 "jump"))))

(deftest abbreviate-test
  (with-instrument `strings/abbreviate
    (is (spec-check strings/abbreviate)))
  (with-instrument (st/instrumentable-syms)
    (is= "hello w..." (strings/abbreviate "hello what's up?" 10))
    (is= "hello" (strings/abbreviate "hello" 10))))
