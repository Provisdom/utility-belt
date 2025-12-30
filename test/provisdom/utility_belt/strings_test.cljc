(ns provisdom.utility-belt.strings-test
  #?(:cljs (:require-macros
             [provisdom.test.core]))
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.strings :as strings]))

;2 seconds

#?(:clj (set! *warn-on-reflection* true))

(t/deftest maybe-keyword-to-string-test
  (t/with-instrument `strings/maybe-keyword-to-string
    (t/is-spec-check strings/maybe-keyword-to-string))
  (t/with-instrument :all
    (t/is= "hello" (strings/maybe-keyword-to-string :hello))
    (t/is= "hello" (strings/maybe-keyword-to-string "hello"))))

(t/deftest substring-test
  (t/with-instrument `strings/substring
    (t/is-spec-check strings/substring))
  (t/with-instrument :all
    (t/is= "hi" (strings/substring "hi" 0))
    (t/is= "ie" (strings/substring "hie" 1 2))))

(t/deftest trim-end-test
  (t/with-instrument `strings/trim-end
    (t/is-spec-check strings/trim-end))
  (t/with-instrument :all
    (t/is= "apple bean " (strings/trim-end "apple bean pear" "pear"))
    (t/is= "apple bean pea" (strings/trim-end "apple bean pearrrrr" "r"))))

(t/deftest trim-start-test
  (t/with-instrument `strings/trim-start
    (t/is-spec-check strings/trim-start))
  (t/with-instrument :all
    (t/is= " bean pear" (strings/trim-start "apple bean pear" "apple"))
    (t/is= "pple bean pear" (strings/trim-start "aaaapple bean pear" "a"))))

(t/deftest rest-string-test
  (t/with-instrument `strings/rest-string
    (t/is-spec-check strings/rest-string))
  (t/with-instrument :all
    (t/is= "i you" (strings/rest-string "hi you"))
    (t/is= "" (strings/rest-string "h"))
    (t/is= "" (strings/rest-string ""))))

(t/deftest butlast-string-test
  (t/with-instrument `strings/butlast-string
    (t/is-spec-check strings/butlast-string))
  (t/with-instrument :all
    (t/is= "hi yo" (strings/butlast-string "hi you"))
    (t/is= "" (strings/butlast-string "u"))
    (t/is= "" (strings/butlast-string ""))))

(t/deftest insert-test
  (t/with-instrument `strings/insert
    (t/is-spec-check strings/insert))
  (t/with-instrument :all
    (t/is= "hi yjumpou" (strings/insert "hi you" 4 "jump"))))

(t/deftest abbreviate-test
  (t/with-instrument `strings/abbreviate
    (t/is-spec-check strings/abbreviate))
  (t/with-instrument :all
    (t/is= "hello w..." (strings/abbreviate "hello what's up?" 10))
    (t/is= "hello" (strings/abbreviate "hello" 10))))

(t/deftest blank->nil-test
  (t/with-instrument `strings/blank->nil
    (t/is-spec-check strings/blank->nil))
  (t/with-instrument :all
    ;; empty string
    (t/is= nil (strings/blank->nil ""))
    ;; whitespace only
    (t/is= nil (strings/blank->nil "   "))
    (t/is= nil (strings/blank->nil "\t\n"))
    ;; non-blank
    (t/is= "hello" (strings/blank->nil "hello"))
    (t/is= " hello " (strings/blank->nil " hello "))
    ;; nil input
    (t/is= nil (strings/blank->nil nil))))

(t/deftest truncate-words-test
  (t/with-instrument `strings/truncate-words
    (t/is-spec-check strings/truncate-words))
  (t/with-instrument :all
    ;; truncate at word boundary
    (t/is= "The..." (strings/truncate-words "The quick brown fox" 10))
    ;; no truncation needed
    (t/is= "Hello world" (strings/truncate-words "Hello world" 20))
    ;; single long word - cuts mid-word to fit max-length
    (t/is= "Superlo..." (strings/truncate-words "Superlongword here" 10))
    ;; exact length
    (t/is= "Hello" (strings/truncate-words "Hello" 5))))

(t/deftest ensure-prefix-test
  (t/with-instrument `strings/ensure-prefix
    (t/is-spec-check strings/ensure-prefix))
  (t/with-instrument :all
    ;; adds prefix when missing
    (t/is= "/foo" (strings/ensure-prefix "foo" "/"))
    ;; no change when already present
    (t/is= "/foo" (strings/ensure-prefix "/foo" "/"))
    ;; empty prefix
    (t/is= "foo" (strings/ensure-prefix "foo" ""))
    ;; multi-char prefix
    (t/is= "http://example.com" (strings/ensure-prefix "example.com" "http://"))))

(t/deftest ensure-suffix-test
  (t/with-instrument `strings/ensure-suffix
    (t/is-spec-check strings/ensure-suffix))
  (t/with-instrument :all
    ;; adds suffix when missing
    (t/is= "foo/" (strings/ensure-suffix "foo" "/"))
    ;; no change when already present
    (t/is= "foo/" (strings/ensure-suffix "foo/" "/"))
    ;; empty suffix
    (t/is= "foo" (strings/ensure-suffix "foo" ""))
    ;; multi-char suffix
    (t/is= "file.txt" (strings/ensure-suffix "file" ".txt"))))

(t/deftest kebab->camel-test
  (t/with-instrument `strings/kebab->camel
    (t/is-spec-check strings/kebab->camel))
  (t/with-instrument :all
    (t/is= "myVariableName" (strings/kebab->camel "my-variable-name"))
    (t/is= "already" (strings/kebab->camel "already"))
    (t/is= "" (strings/kebab->camel ""))
    (t/is= "aBC" (strings/kebab->camel "a-b-c"))))

(t/deftest camel->kebab-test
  (t/with-instrument `strings/camel->kebab
    (t/is-spec-check strings/camel->kebab))
  (t/with-instrument :all
    (t/is= "my-variable-name" (strings/camel->kebab "myVariableName"))
    (t/is= "already" (strings/camel->kebab "already"))
    (t/is= "" (strings/camel->kebab ""))
    ;; all-uppercase has no lowercase-to-uppercase transitions
    (t/is= "abc" (strings/camel->kebab "ABC"))
    ;; consecutive uppercase stays together after the first transition
    (t/is= "my-url" (strings/camel->kebab "myURL"))))

(t/deftest kebab->snake-test
  (t/with-instrument `strings/kebab->snake
    (t/is-spec-check strings/kebab->snake))
  (t/with-instrument :all
    (t/is= "my_variable_name" (strings/kebab->snake "my-variable-name"))
    (t/is= "already" (strings/kebab->snake "already"))
    (t/is= "" (strings/kebab->snake ""))))

(t/deftest snake->kebab-test
  (t/with-instrument `strings/snake->kebab
    (t/is-spec-check strings/snake->kebab))
  (t/with-instrument :all
    (t/is= "my-variable-name" (strings/snake->kebab "my_variable_name"))
    (t/is= "already" (strings/snake->kebab "already"))
    (t/is= "" (strings/snake->kebab ""))))

(t/deftest pad-left-test
  (t/with-instrument `strings/pad-left
    (t/is-spec-check strings/pad-left))
  (t/with-instrument :all
    (t/is= "00042" (strings/pad-left "42" 5 \0))
    (t/is= "     hello" (strings/pad-left "hello" 10))
    (t/is= "toolong" (strings/pad-left "toolong" 3 \0))
    (t/is= "x" (strings/pad-left "x" 1))))

(t/deftest pad-right-test
  (t/with-instrument `strings/pad-right
    (t/is-spec-check strings/pad-right))
  (t/with-instrument :all
    (t/is= "42000" (strings/pad-right "42" 5 \0))
    (t/is= "hello     " (strings/pad-right "hello" 10))
    (t/is= "toolong" (strings/pad-right "toolong" 3 \0))
    (t/is= "x" (strings/pad-right "x" 1))))

(t/deftest normalize-whitespace-test
  (t/with-instrument `strings/normalize-whitespace
    (t/is-spec-check strings/normalize-whitespace))
  (t/with-instrument :all
    (t/is= "hello world" (strings/normalize-whitespace "  hello   world  "))
    (t/is= "line1 line2" (strings/normalize-whitespace "line1\n\n\nline2"))
    (t/is= "" (strings/normalize-whitespace "   "))
    (t/is= "single" (strings/normalize-whitespace "single"))))

(t/deftest word-count-test
  (t/with-instrument `strings/word-count
    (t/is-spec-check strings/word-count))
  (t/with-instrument :all
    (t/is= 2 (strings/word-count "hello world"))
    (t/is= 2 (strings/word-count "  multiple   spaces  "))
    (t/is= 0 (strings/word-count ""))
    (t/is= 0 (strings/word-count "   "))
    (t/is= 1 (strings/word-count "single"))))

(t/deftest capitalize-words-test
  (t/with-instrument `strings/capitalize-words
    (t/is-spec-check strings/capitalize-words))
  (t/with-instrument :all
    (t/is= "Hello World" (strings/capitalize-words "hello world"))
    (t/is= "The Quick Brown Fox" (strings/capitalize-words "the quick brown fox"))
    (t/is= "" (strings/capitalize-words ""))
    (t/is= "Already Capitalized" (strings/capitalize-words "already capitalized"))))

(t/deftest truncate-middle-test
  (t/with-instrument `strings/truncate-middle
    (t/is-spec-check strings/truncate-middle))
  (t/with-instrument :all
    (t/is= "short" (strings/truncate-middle "short" 20))
    ;; truncates in middle: 16 chars = 6 start + "..." + 7 end
    (t/is= "/Users...ile.txt" (strings/truncate-middle "/Users/foo/very/long/path/to/file.txt" 16))
    ;; minimum length enforced: 5 chars = 1 start + "..." + 1 end
    (t/is= "h...d" (strings/truncate-middle "hello world" 5))))
