(ns provisdom.utility-belt.strings
  "Extended string manipulation utilities that complement clojure.string.
  Provides functions for substring operations, trimming, insertion, and abbreviation with full spec
  definitions for validation and testing."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]))

;;;String.format "%[argument number] [flags] [width] [.precision] type"
;;;examples: (format "%02d" long) returns a string with at least 
;;;   two places with a leading zero if necessary
;;;http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html

(defn- not-empty?
  [x]
  (not (empty? x)))

(s/def ::not-empty-string
  (s/and string? not-empty?))

(def ^:private max-index
  #?(:clj Integer/MAX_VALUE
     :cljs (.-MAX_SAFE_INTEGER js/Number)))

(s/def ::index
  (s/with-gen
    (s/int-in 0 max-index)
    #(gen/large-integer* {:max 6 :min 0})))

;;;STRING MANIPULATION
(defn maybe-keyword-to-string
  "If `x` is a keyword, returns string. Otherwise, returns `x`."
  [x]
  (if (keyword? x)
    (name x)
    x))

(s/fdef maybe-keyword-to-string
  :args (s/cat :x any?)
  :ret any?)

(defn substring
  "Returns substring of string `s` starting at `start-index` and ending at optional `end-index`."
  ([s start-index]
   (let [s (str s)
         c (count s)
         start (int start-index)]
     (if (>= start c)
       ""
       (subs s start))))
  ([s start-index end-index]
   (let [s (str s)
         c (count s)
         start (int start-index)
         end (min (inc (int end-index)) c)]
     (if (or (>= start c) (<= end start))
       ""
       (subs s start end)))))

(s/fdef substring
  :args (s/cat :s string?
          :start-index ::index
          :end-index (s/? ::index))
  :ret string?)

(defn trim-end
  "Recursively trims all occurrences of `suffix` from the end of string `s`.

   This function removes all consecutive occurrences of the suffix from the end, not just a single
   occurrence.

   Parameters:
   - s: The string to trim
   - suffix: The suffix to remove

   Examples:
   ```clojure
   (trim-end \"hello!!!\" \"!\")
   ;; => \"hello\"

   (trim-end \"test.txt.txt\" \".txt\")
   ;; => \"test\"
   ```"
  [s suffix]
  (let [s (str s)
        count-suffix (count suffix)
        c (count s)]
    (cond
      (= s (str suffix)) ""

      (and (str/ends-with? s suffix) (< count-suffix c) (pos? count-suffix))
      (trim-end (substring s 0 (- c count-suffix 1)) suffix)

      :else s)))

(s/fdef trim-end
  :args (s/cat :s string? :suffix string?)
  :ret string?)

(defn trim-start
  "Recursively trims all occurrences of `prefix` from the start of string `s`.

   This function removes all consecutive occurrences of the prefix from the beginning, not just a
   single occurrence.

   Parameters:
   - s: The string to trim
   - prefix: The prefix to remove

   Examples:
   ```clojure
   (trim-start \"###title\" \"#\")
   ;; => \"title\"

   (trim-start \"http://http://example.com\" \"http://\")
   ;; => \"example.com\"
   ```"
  [s prefix]
  (let [s (str s)
        count-prefix (count prefix)
        c (count s)]
    (cond
      (= s (str prefix)) ""

      (and (str/starts-with? s prefix) (< count-prefix c) (pos? count-prefix))
      (trim-start (substring s count-prefix (dec c)) prefix)

      :else s)))

(s/fdef trim-start
  :args (s/cat :s string? :prefix string?)
  :ret string?)

(defn rest-string
  "Returns a possibly empty string of characters after the first."
  [s]
  (if (empty? s)
    ""
    (subs s 1)))

(s/fdef rest-string
  :args (s/cat :s string?)
  :ret string?)

(defn butlast-string
  "Returns a possibly empty string of characters except the last."
  [s]
  (let [len (count s)]
    (if (zero? len)
      ""
      (subs s 0 (dec len)))))

(s/fdef butlast-string
  :args (s/cat :s string?)
  :ret string?)

(defn insert
  "Inserts a `substring` into a string `s` at `index`."
  [s index substring]
  (let [[st e] (split-at index s)]
    (str/join (concat st substring e))))

(s/fdef insert
  :args (s/cat :s string?
          :index ::index
          :substring string?)
  :ret string?)

(defn abbreviate
  "Shortens a string to the specified maximum length, adding ellipses if truncated.

   Ensures the resulting string doesn't exceed `max-length` by replacing the end with an ellipsis
   marker (\"...\") if necessary. Will always leave at least two characters from the original string
   plus the marker.

   Parameters:
   - s: The string to abbreviate
   - max-length: The maximum length for the resulting string (minimum value is 5)

   Examples:
   ```clojure
   (abbreviate \"This is a long string\" 10)
   ;; => \"This i...\"

   (abbreviate \"Short\" 10)
   ;; => \"Short\" ; No change needed
   ```"
  [s max-length]
  (let [max-length (max 5 max-length)
        abbrev-marker "..."]
    (if (<= (count s) max-length)
      s
      (str (substring s 0 (- max-length 4)) abbrev-marker))))

(s/fdef abbreviate
  :args (s/cat :s string?
          :max-length ::index)
  :ret string?)

(defn blank->nil
  "Converts blank strings to nil. Returns non-blank strings unchanged.

   A string is considered blank if it is nil, empty, or contains only whitespace.

   Parameters:
   - s: The string to check

   Examples:
   ```clojure
   (blank->nil \"\")
   ;; => nil

   (blank->nil \"   \")
   ;; => nil

   (blank->nil \"hello\")
   ;; => \"hello\"

   (blank->nil nil)
   ;; => nil
   ```"
  [s]
  (when-not (str/blank? s)
    s))

(s/fdef blank->nil
  :args (s/cat :s (s/nilable string?))
  :ret (s/nilable ::not-empty-string))

(defn truncate-words
  "Truncates a string at word boundary to fit within max-length.

   Unlike `abbreviate` which can cut mid-word, this function respects word boundaries. If truncation
   occurs, adds an ellipsis marker.

   Parameters:
   - s: The string to truncate
   - max-length: The maximum length for the result (minimum 5)

   Examples:
   ```clojure
   (truncate-words \"The quick brown fox\" 10)
   ;; => \"The...\"

   (truncate-words \"Hello world\" 20)
   ;; => \"Hello world\"

   (truncate-words \"Superlongword here\" 10)
   ;; => \"Super...\"
   ```"
  [s max-length]
  (let [max-length (max 5 max-length)
        marker "..."]
    (if (<= (count s) max-length)
      s
      (let [available (- max-length (count marker))
            truncated (subs s 0 available)
            last-space (str/last-index-of truncated " ")]
        (if (and last-space (pos? last-space))
          (str (str/trimr (subs truncated 0 last-space)) marker)
          (str (str/trimr truncated) marker))))))

(s/fdef truncate-words
  :args (s/cat :s string?
          :max-length ::index)
  :ret string?)

(defn ensure-prefix
  "Ensures string `s` starts with `prefix`. Adds `prefix` if not already present.

   Parameters:
   - s: The string to check
   - prefix: The prefix to ensure

   Examples:
   ```clojure
   (ensure-prefix \"foo\" \"/\")
   ;; => \"/foo\"

   (ensure-prefix \"/foo\" \"/\")
   ;; => \"/foo\"
   ```"
  [s prefix]
  (if (str/starts-with? s prefix)
    s
    (str prefix s)))

(s/fdef ensure-prefix
  :args (s/cat :s string? :prefix string?)
  :ret string?)

(defn ensure-suffix
  "Ensures string `s` ends with `suffix`. Adds `suffix` if not already present.

   Parameters:
   - s: The string to check
   - suffix: The suffix to ensure

   Examples:
   ```clojure
   (ensure-suffix \"foo\" \"/\")
   ;; => \"foo/\"

   (ensure-suffix \"foo/\" \"/\")
   ;; => \"foo/\"
   ```"
  [s suffix]
  (if (str/ends-with? s suffix)
    s
    (str s suffix)))

(s/fdef ensure-suffix
  :args (s/cat :s string? :suffix string?)
  :ret string?)

;;;CASE CONVERSIONS
(defn kebab->camel
  "Converts kebab-case string to camelCase.

   Examples:
   ```clojure
   (kebab->camel \"my-variable-name\")
   ;; => \"myVariableName\"

   (kebab->camel \"already\")
   ;; => \"already\"
   ```"
  [s]
  (let [parts (str/split s #"-")]
    (str (first parts)
      (str/join (map str/capitalize (rest parts))))))

(s/fdef kebab->camel
  :args (s/cat :s string?)
  :ret string?)

(defn camel->kebab
  "Converts camelCase string to kebab-case.

   Examples:
   ```clojure
   (camel->kebab \"myVariableName\")
   ;; => \"my-variable-name\"

   (camel->kebab \"already\")
   ;; => \"already\"
   ```"
  [s]
  (-> s
    (str/replace #"([a-z])([A-Z])" "$1-$2")
    str/lower-case))

(s/fdef camel->kebab
  :args (s/cat :s string?)
  :ret string?)

(defn kebab->snake
  "Converts kebab-case string to snake_case.

   Examples:
   ```clojure
   (kebab->snake \"my-variable-name\")
   ;; => \"my_variable_name\"
   ```"
  [s]
  (str/replace s "-" "_"))

(s/fdef kebab->snake
  :args (s/cat :s string?)
  :ret string?)

(defn snake->kebab
  "Converts snake_case string to kebab-case.

   Examples:
   ```clojure
   (snake->kebab \"my_variable_name\")
   ;; => \"my-variable-name\"
   ```"
  [s]
  (str/replace s "_" "-"))

(s/fdef snake->kebab
  :args (s/cat :s string?)
  :ret string?)

;;;PADDING
(defn pad-left
  "Pads string `s` on the left to reach `length` using `pad-char`.

   Parameters:
   - s: The string to pad
   - length: The target length
   - pad-char: The character to use for padding (default: space)

   Examples:
   ```clojure
   (pad-left \"42\" 5 \\0)
   ;; => \"00042\"

   (pad-left \"hello\" 10)
   ;; => \"     hello\"

   (pad-left \"toolong\" 3 \\0)
   ;; => \"toolong\"
   ```"
  ([s length]
   (pad-left s length \space))
  ([s length pad-char]
   (let [s (str s)
         pad-needed (- length (count s))]
     (if (pos? pad-needed)
       (str (apply str (repeat pad-needed pad-char)) s)
       s))))

(s/fdef pad-left
  :args (s/cat :s string?
          :length ::index
          :pad-char (s/? char?))
  :ret string?)

(defn pad-right
  "Pads string `s` on the right to reach `length` using `pad-char`.

   Parameters:
   - s: The string to pad
   - length: The target length
   - pad-char: The character to use for padding (default: space)

   Examples:
   ```clojure
   (pad-right \"42\" 5 \\0)
   ;; => \"42000\"

   (pad-right \"hello\" 10)
   ;; => \"hello     \"

   (pad-right \"toolong\" 3 \\0)
   ;; => \"toolong\"
   ```"
  ([s length]
   (pad-right s length \space))
  ([s length pad-char]
   (let [s (str s)
         pad-needed (- length (count s))]
     (if (pos? pad-needed)
       (str s (apply str (repeat pad-needed pad-char)))
       s))))

(s/fdef pad-right
  :args (s/cat :s string?
          :length ::index
          :pad-char (s/? char?))
  :ret string?)

;;;WHITESPACE & WORDS
(defn normalize-whitespace
  "Collapses multiple consecutive whitespace characters into a single space and trims
  leading/trailing whitespace.

   Examples:
   ```clojure
   (normalize-whitespace \"  hello   world  \")
   ;; => \"hello world\"

   (normalize-whitespace \"line1\\n\\n\\nline2\")
   ;; => \"line1 line2\"
   ```"
  [s]
  (-> s
    (str/replace #"\s+" " ")
    str/trim))

(s/fdef normalize-whitespace
  :args (s/cat :s string?)
  :ret string?)

(defn word-count
  "Counts the number of words in a string.

   Words are separated by whitespace. Empty strings return 0.

   Examples:
   ```clojure
   (word-count \"hello world\")
   ;; => 2

   (word-count \"  multiple   spaces  \")
   ;; => 2

   (word-count \"\")
   ;; => 0
   ```"
  [s]
  (if (str/blank? s)
    0
    (count (str/split (str/trim s) #"\s+"))))

(s/fdef word-count
  :args (s/cat :s string?)
  :ret nat-int?)

(defn capitalize-words
  "Capitalizes the first letter of each word in a string.

   Examples:
   ```clojure
   (capitalize-words \"hello world\")
   ;; => \"Hello World\"

   (capitalize-words \"the quick brown fox\")
   ;; => \"The Quick Brown Fox\"
   ```"
  [s]
  (->> (str/split s #"(?<=\s)|(?=\s)")
    (map #(if (str/blank? %) % (str/capitalize %)))
    str/join))

(s/fdef capitalize-words
  :args (s/cat :s string?)
  :ret string?)

(defn truncate-middle
  "Truncates a string in the middle, keeping the start and end.

   Useful for displaying long file paths or URLs while preserving the beginning and end which are
   often most informative.

   Parameters:
   - s: The string to truncate
   - max-length: The maximum length for the result (minimum 5)

   Examples:
   ```clojure
   (truncate-middle \"/Users/foo/very/long/path/to/file.txt\" 25)
   ;; => \"/Users/foo/.../file.txt\"

   (truncate-middle \"short\" 20)
   ;; => \"short\"
   ```"
  [s max-length]
  (let [max-length (max 5 max-length)
        len (count s)]
    (if (<= len max-length)
      s
      (let [marker "..."
            available (- max-length (count marker))
            half (quot available 2)
            start-len half
            end-len (- available half)]
        (str (subs s 0 start-len) marker (subs s (- len end-len)))))))

(s/fdef truncate-middle
  :args (s/cat :s string?
          :max-length ::index)
  :ret string?)
