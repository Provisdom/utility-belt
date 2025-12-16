(ns provisdom.utility-belt.strings-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.strings :as strings]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest maybe-keyword-to-string-test
  (t/with-instrument `strings/maybe-keyword-to-string
    (is (t/spec-check strings/maybe-keyword-to-string)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hello" (strings/maybe-keyword-to-string :hello))
    (t/is= "hello" (strings/maybe-keyword-to-string "hello"))))

(deftest substring-test
  (t/with-instrument `strings/substring
    (is (t/spec-check strings/substring)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi" (strings/substring "hi" 0))
    (t/is= "ie" (strings/substring "hie" 1 2))))

(deftest trim-end-test
  (t/with-instrument `strings/trim-end
    (is (t/spec-check strings/trim-end)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
    (t/is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r"))))

(deftest trim-start-test
  (t/with-instrument `strings/trim-start
    (is (t/spec-check strings/trim-start)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
    (t/is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a"))))

(deftest rest-string-test
  (t/with-instrument `strings/rest-string
    (is (t/spec-check strings/rest-string)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "i you" (strings/rest-string "hi you"))
    (t/is= "" (strings/rest-string "h"))
    (t/is= "" (strings/rest-string ""))))

(deftest butlast-string-test
  (t/with-instrument `strings/butlast-string
    (is (t/spec-check strings/butlast-string)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi yo" (strings/butlast-string "hi you"))
    (t/is= "" (strings/butlast-string "u"))
    (t/is= "" (strings/butlast-string ""))))

(deftest insert-test
  (t/with-instrument `strings/insert
    (is (t/spec-check strings/insert)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi yjumpou" (strings/insert "hi you" 4 "jump"))))

(deftest abbreviate-test
  (t/with-instrument `strings/abbreviate
    (is (t/spec-check strings/abbreviate)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hello w..." (strings/abbreviate "hello what's up?" 10))
    (t/is= "hello" (strings/abbreviate "hello" 10))))
