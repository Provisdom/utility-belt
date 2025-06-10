(ns provisdom.utility-belt.strings
  "Extended string manipulation utilities that complement clojure.string.
   Provides functions for substring operations, trimming, insertion, and abbreviation
   with full spec definitions for validation and testing."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]))

;;;TODO - need for CLJS too

;;;String.format "%[argument number] [flags] [width] [.precision] type"
;;;examples: (format "%02d" long) returns a string with at least 
;;;   two places with a leading zero if necessary
;;;http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html

(defn- not-empty?
  [x]
  (not (empty? x)))

(s/def ::not-empty-string
  (s/and string? not-empty?))

(s/def ::index
  (s/with-gen
    (s/int-in 0 Integer/MAX_VALUE)
    #(gen/large-integer* {:min 0 :max 6})))

;;;STRING MANIPULATION
(defn maybe-keyword-to-string
  "If `x` is a keyword, returns string. Otherwise, returns `x`."
  [x]
  (if (keyword? x)
    (str/join (rest (str x)))
    x))

(s/fdef maybe-keyword-to-string
  :args (s/cat :x any?)
  :ret any?)

(defn substring
  "Returns substring of string `s` starting at `start-index` and ending at
  optional `end-index`."
  ([s start-index]
   (let [s (str s)
         c (count s)
         start (int start-index)]
     (if (>= start c)
       ""
       (.substring s start))))
  ([s start-index end-index]
   (let [s (str s)
         c (count s)
         start (int start-index)
         end (min (inc (int end-index)) c)]
     (if (or (>= start c) (<= end start))
       ""
       (.substring s start end)))))

(s/fdef substring
  :args (s/cat :s string?
          :start-index ::index
          :end-index (s/? ::index))
  :ret string?)

(defn trim-end
  "Recursively trims all occurrences of `suffix` from the end of string `s`.
   
   Unlike clojure.string/trim-suffix which only removes a single occurrence,
   this function removes all occurrences from the end.
   
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

      (and (str/ends-with? s suffix)
        (< count-suffix c)
        (pos? count-suffix))
      (trim-end
        (substring s 0 (- c count-suffix 1))
        suffix)

      :else s)))

(s/fdef trim-end
  :args (s/cat :s string? :suffix string?)
  :ret string?)

(defn trim-start
  "Recursively trims all occurrences of `prefix` from the start of string `s`.
   
   Unlike clojure.string/trim-newline which only removes a single occurrence,
   this function removes all occurrences from the beginning.
   
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

      (and (str/starts-with? s prefix)
        (< count-prefix c)
        (pos? count-prefix))
      (trim-start
        (substring s count-prefix (dec c))
        prefix)

      :else s)))

(s/fdef trim-start
  :args (s/cat :s string? :prefix string?)
  :ret string?)

(defn rest-string
  "Returns a possibly empty string of characters after the first."
  [s]
  (str/join (rest s)))

(s/fdef rest-string
  :args (s/cat :s string?)
  :ret string?)

(defn butlast-string
  "Returns a possibly empty string of characters except the last."
  [s]
  (str/join (butlast s)))

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
   
   Ensures the resulting string doesn't exceed `max-length` by replacing the end
   with an ellipsis marker (\"...\") if necessary. Will always leave at least two 
   characters from the original string plus the marker.
   
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
