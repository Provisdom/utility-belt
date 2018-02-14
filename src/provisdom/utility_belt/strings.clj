(ns provisdom.utility-belt.strings
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.string :as str])
  (:import
    [org.apache.commons.lang3 StringUtils]))

;;;TODO - need for CLJS too

;;;String.format "%[argument number] [flags] [width] [.precision] type"
;;;examples: (format "%02d" long) returns a string with at least 
;;;   two places with a leading zero if necessary
;;;http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html

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
  ([s start-index] (.substring (str s) (int start-index)))
  ([s start-index end-index]
   (.substring (str s) (int start-index) (inc (int end-index)))))

(s/fdef substring
        :args (s/cat :s string? :start-index ::index :end-index (s/? ::index))
        :ret string?)

(defn trim-end
  "Trims all the `suffix` from the end of string `s`."
  [s suffix]
  (let [s (str s)]
    (if-not (str/ends-with? s suffix)
      s
      (trim-end (substring s 0 (- (count s) (count suffix) 1)) suffix))))

(s/fdef trim-end
        :args (s/cat :s string? :suffix string?)
        :ret string?)

(defn trim-start
  "Trims all the `prefix` from the start of string `s`."
  [s prefix]
  (let [s (str s)]
    (if-not (str/starts-with? s prefix)
      s
      (trim-start (substring s (count prefix) (dec (count s))) prefix))))

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
        :args (s/cat :s string? :index ::index :substring string?)
        :ret string?)

(defn abbreviate
  "Shortens string `s` to `max-length` using ellipses if needed."
  [s max-length]
  (let [max-length (max 5 max-length)]
    (StringUtils/abbreviate (str s) (int max-length))))

(s/fdef abbreviate
        :args (s/cat :s string? :max-length ::index)
        :ret string?)