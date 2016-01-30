(ns provisdom.test.utility-belt.format
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.utility-belt.format :refer :all]))

;;;REDUNDANCY from clojure.string
;;;replace-string (replace), replace-first, join, split

(facts "string manipulation"
       (fact "substring?"
             (substring? "hi" "are hi? you") => true
             (substring? "hie" "are hi? you") => false)
       (fact "maybe-keyword-to-string"
             (maybe-keyword-to-string "A") => "A" 
             (maybe-keyword-to-string :A) => "A"
             (maybe-keyword-to-string 1) => 1)
       (fact "ends-with?"
             (ends-with? "apple bean pear" "ear") => true
             (ends-with? "apple bean pear" "ears") => false)
       (fact "starts-with?"
             (starts-with? "apple bean pear" "app") => true
             (starts-with? "apple bean pear" "bap") => false)
       (fact "substring"
             (substring "apple bean pear" 8) => "an pear"
             (substring "apple bean pear" 7 12) => "ean pe")
       (fact "trim-end"
             (trim-end "apple bean pear" "pear") => "apple bean "
             (trim-end "apple bean pearrrrr" "r") => "apple bean pea")
       (fact "trim-start"
             (trim-start "apple bean pear" "apple") => " bean pear"
             (trim-start "aaaapple bean pear" "a") => "pple bean pear")
       (fact "rest-string"
             (rest-string "hi you") => "i you"
             (rest-string "h") => ""
             (rest-string "") => "")
       (fact "butlast-string"
             (butlast-string "hi you") => "hi yo"
             (butlast-string "u") => ""
             (butlast-string "") => "")
       (fact "insert"
             (insert "hi you" 4 "jump") => "hi yjumpou")
       (fact "abbreviate"
             (abbreviate "hello what's up?" 10) => "hello w..."
             (abbreviate "hello" 10) => "hello"))
