(ns provisdom.utility-belt.strings-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.strings :as strings]))

;1 seconds

(set! *warn-on-reflection* true)

(ct/deftest maybe-keyword-to-string-test
  (t/with-instrument `strings/maybe-keyword-to-string
    (t/is-spec-check strings/maybe-keyword-to-string))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hello" (strings/maybe-keyword-to-string :hello))
    (t/is= "hello" (strings/maybe-keyword-to-string "hello"))))

(ct/deftest substring-test
  (t/with-instrument `strings/substring
    (t/is-spec-check strings/substring))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi" (strings/substring "hi" 0))
    (t/is= "ie" (strings/substring "hie" 1 2))))

(ct/deftest trim-end-test
  (t/with-instrument `strings/trim-end
    (t/is-spec-check strings/trim-end))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
    (t/is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r"))))

(ct/deftest trim-start-test
  (t/with-instrument `strings/trim-start
    (t/is-spec-check strings/trim-start))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
    (t/is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a"))))

(ct/deftest rest-string-test
  (t/with-instrument `strings/rest-string
    (t/is-spec-check strings/rest-string))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "i you" (strings/rest-string "hi you"))
    (t/is= "" (strings/rest-string "h"))
    (t/is= "" (strings/rest-string ""))))

(ct/deftest butlast-string-test
  (t/with-instrument `strings/butlast-string
    (t/is-spec-check strings/butlast-string))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi yo" (strings/butlast-string "hi you"))
    (t/is= "" (strings/butlast-string "u"))
    (t/is= "" (strings/butlast-string ""))))

(ct/deftest insert-test
  (t/with-instrument `strings/insert
    (t/is-spec-check strings/insert))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hi yjumpou" (strings/insert "hi you" 4 "jump"))))

(ct/deftest abbreviate-test
  (t/with-instrument `strings/abbreviate
    (t/is-spec-check strings/abbreviate))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "hello w..." (strings/abbreviate "hello what's up?" 10))
    (t/is= "hello" (strings/abbreviate "hello" 10))))
