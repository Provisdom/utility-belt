(ns provisdom.utility-belt.format
  (:require [clojure.string :as str])
  (:import [org.apache.commons.lang3 StringUtils]))

;;;TODO - need for CLJS too

(set! *warn-on-reflection* true)

;;;String.format "%[argument number] [flags] [width] [.precision] type"
;;;examples: (format "%02d" long) returns a string with at least 
;;;   two places with a leading zero if necessary
;;;http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html

;;;REDUNDANCY
(defn replace-string
  "Replaces all instance of match with replacement in s.
match/replacement can be:
   string / string
   char / char
   pattern / (string or function of match).
See also replace-first.
The replacement is literal (i.e. none of its characters are treated specially) 
   for all cases above except pattern / string.
For pattern / string, $1, $2, etc. in the replacement string are substituted 
   with the string that matched the corresponding parenthesized group in the 
   pattern.  
If you wish your replacement string r to be used literally, use (re-quote-
   replacement r) as the replacement argument.  
See also documentation for java.util.regex.Matcher's appendReplacement method."
  [s match replacement] (str/replace s match replacement))

(defn replace-first
  "Replaces the first instance of match with replacement in s."
  [s match replacement] (str/replace-first s match replacement))

(defn join
  "Returns a string of all elements in coll, as returned by (seq coll),
   separated by an optional separator."
  ([coll] (str/join coll))
  ([separator coll] (str/join separator coll)))

(defn split
  "Splits string on a regular expression. 
   Optional argument limit is the maximum number of splits. 
   Not lazy. Returns vector of the splits. 
   Regex begins with # and is in quotes."
  ([s re] (str/split s re))
  ([s re limit] (str/split s re limit)))

(defn ends-with?
  "Tests whether s ends with suffix"
  [s suffix] (str/ends-with? (str s) (str suffix)))

(defn starts-with?
  "Tests whether s starts with prefix"
  [s prefix] (str/starts-with? (str s) (str prefix)))

(defn substring?
  "True if s contains the substring."
  [sub s] (str/includes? (str s) sub))

(defn index-of
  "Return index of value (string or char) in s, optionally searching forward from from-index or nil if not found."
  ([s value]
   (str/index-of s value))
  ([s value ^long from-index]
   (str/index-of s value from-index)))

(defn last-index-of
  "Return last index of value (string or char) in s, optionally searching backward from from-index or nil if not found."
  ([s value]
   (str/last-index-of s value))
  ([s value ^long from-index]
   (str/last-index-of s value from-index)))

;;;STRING MANIPULATION
(defn maybe-keyword-to-string 
  "If 'x' is a keyword, returns string.
Otherwise, returns 'x'"
  [x] (if (keyword? x) (str/join (rest (str x))) x))

(defn substring
  "Returns substring of s by index"
  ([s ^long start-index] (.substring (str s) start-index))
  ([s ^long start-index ^long end-index] 
    (.substring (str s) start-index (inc end-index))))

(defn trim-end 
  "Trims all the 'suffix' from the end"
  [s suffix]
  (let [s (str s)]
    (if-not (ends-with? s suffix) s
      (trim-end (substring s 0 (- (count s) (count suffix) 1)) suffix))))

(defn trim-start 
  "Trims all the 'prefix' from the start"
  [s prefix]
  (let [s (str s)]
    (if-not (starts-with? s prefix) s
      (trim-start (substring s (count prefix) (dec (count s))) prefix))))

(defn rest-string 
  "Returns a possibly empty string of characters after the first."
  [s] (join (rest s)))

(defn butlast-string 
  "Returns a possibly empty string of characters except the last."
  [s] (join (butlast s)))

(defn insert 
  "Inserts a substring into a string at index"
  [s index sub]
  (let [[st e] (split-at index s)]
    (join (concat st sub e))))

(defn abbreviate 
  "Shortens string to max-length using ellipses if needed.
max length for tree display used to be 21"
  [s ^long max-length]
  (let [max-length (max 5 max-length)]
    (StringUtils/abbreviate (str s) max-length)))