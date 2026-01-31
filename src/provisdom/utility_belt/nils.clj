(ns provisdom.utility-belt.nils
  "Utilities for handling `nil` values in different ways. Provides functions for ignoring,
  reporting, or replacing `nil` values when applying functions or processing collections.

   Core functions:
   - [[ignore-nils]] / [[ignore-nils-fn]] - Remove `nil` args before calling function
   - [[nil-nils]] / [[nil-nils-fn]] - Return `nil` if any arg is `nil`
   - [[anomaly-nils]] / [[anomaly-nils-fn]] - Return anomaly if any arg is `nil`
   - [[coalesce]] - Return first non-nil value
   - [[default-nil]] - Return value or default if `nil`
   - [[remove-nil-vals]] / [[replace-nil-vals]] - Map utilities for `nil` values
   - [[replace-nils]] - Replace nils in sequence with values from another"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.utility-belt.anomalies :as anomalies]))

(defn ignore-nils
  "Applies function `f` and ignores any `nil` values in `args`."
  [f & args]
  (apply f (remove nil? args)))

(s/fdef ignore-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn anomaly-nils
  "Applies function `f` but returns an anomaly if any argument is `nil` or if the result is `nil`.
   This function is useful for workflows where `nil` values should be treated as errors. Uses the
   anomalies system to report these nil-related errors in a standard way.

   Parameters:
   - `f`: The function to apply
   - `args`: Arguments to pass to the function

   Returns:
   - If any arg is `nil`: Returns a `::anomalies/forbidden` anomaly
   - If `f` returns `nil`: Returns a `::anomalies/forbidden` anomaly
   - Otherwise: Returns the result of `(apply f args)`

   Examples:
   ```clojure
   (anomaly-nils + 1 2 3)    ;; => 6
   (anomaly-nils + 1 nil 3)  ;; => {::anomalies/category ::anomalies/forbidden ...}
   (anomaly-nils (constantly nil) 1)  ;; => {::anomalies/category ::anomalies/forbidden ...}
   ```"
  [f & args]
  (let [anomaly {::anomalies/category ::anomalies/forbidden
                 ::anomalies/fn       (var anomaly-nils)
                 ::anomalies/message  "nil not allowed"}]
    (if (some nil? args)
      anomaly
      (or (apply f args) anomaly))))

(s/fdef anomaly-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn nil-nils
  "Applies function `f` but returns `nil` if any argument is `nil`. A simpler alternative to
   [[anomaly-nils]] when you don't need the detailed error reporting of anomalies. Can be used with
   threading macros to short-circuit on `nil` values.

   Parameters:
   - `f`: The function to apply
   - `args`: Arguments to pass to the function

   Returns:
   - If any arg is `nil`: Returns `nil`
   - Otherwise: Returns the result of `(apply f args)`

   Examples:
   ```clojure
   (nil-nils + 1 2 3)    ;; => 6
   (nil-nils + 1 nil 3)  ;; => nil
   ```"
  [f & args]
  (when-not (some nil? args)
    (apply f args)))

(s/fdef nil-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn replace-nils
  "Returns a lazy sequence constructed by replacing `nil` values with values from a replacement
   collection. For each `nil` value found in the input collection, takes the next value from the
   replacement collection. Non-nil values from the input collection are preserved.

   Parameters:
   - `coll`: The input collection that may contain `nil` values
   - `replacement-coll`: Collection of values to use in place of nils

   Returns a lazy sequence with `nil` values replaced by values from `replacement-coll`.

   Examples:
   ```clojure
   (replace-nils [1 nil 3 nil 5] [:a :b])
   ;; => [1 :a 3 :b 5]

   (replace-nils [1 nil 3 nil 5] [:a])
   ;; => [1 :a 3 nil 5] ; replacement values used up
   ```"
  [[h & t :as coll] replacement-coll]
  (if (or (empty? replacement-coll) (empty? coll))
    coll
    (lazy-seq
      (if (nil? h)
        (cons (first replacement-coll) (replace-nils t (rest replacement-coll)))
        (cons h (replace-nils t replacement-coll))))))

(s/fdef replace-nils
  :args (s/cat :coll (s/every any?)
          :replacement-coll (s/every any?))
  :ret (s/every any?))

;;;; Higher-order function wrappers
(defn ignore-nils-fn
  "Returns a function that ignores `nil` args when calling `f`. This is the higher-order version of
  [[ignore-nils]], useful for composition and passing to higher-order functions.

   Examples:
   ```clojure
   ((ignore-nils-fn +) 1 nil 2 nil 3)  ;; => 6
   (map (ignore-nils-fn max) [[1 nil 3] [nil 5 2]])  ;; with apply
   ```"
  [f]
  (fn [& args]
    (apply f (remove nil? args))))

(s/fdef ignore-nils-fn
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list + - * max min)))))
  :ret fn?)

(defn nil-nils-fn
  "Returns a function that returns `nil` if any arg is `nil`. This is the higher-order version of
   [[nil-nils]], useful for composition and passing to higher-order functions.

   Examples:
   ```clojure
   ((nil-nils-fn +) 1 2 3)    ;; => 6
   ((nil-nils-fn +) 1 nil 3)  ;; => nil
   (update m :x (nil-nils-fn inc))
   ```"
  [f]
  (fn [& args]
    (when-not (some nil? args)
      (apply f args))))

(s/fdef nil-nils-fn
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list + - * max min)))))
  :ret fn?)

(defn anomaly-nils-fn
  "Returns a function that returns an anomaly if any arg is `nil` or if the result is `nil`. This
   is the higher-order version of [[anomaly-nils]], useful for composition and passing to
   higher-order functions.

   Examples:
   ```clojure
   ((anomaly-nils-fn +) 1 2 3)    ;; => 6
   ((anomaly-nils-fn +) 1 nil 3)  ;; => {::anomalies/category ::anomalies/forbidden ...}
   ```"
  [f]
  (fn [& args]
    (let [anomaly {::anomalies/category ::anomalies/forbidden
                   ::anomalies/fn       (var anomaly-nils-fn)
                   ::anomalies/message  "nil not allowed"}]
      (if (some nil? args)
        anomaly
        (or (apply f args) anomaly)))))

(s/fdef anomaly-nils-fn
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list + - * max min)))))
  :ret fn?)

;;;; Nil coalescing and defaults
(defn coalesce
  "Returns the first non-nil value, or `nil` if all values are `nil`. Unlike `or`, this only
   checks for `nil`, not falsiness. `false` is returned if it's the first non-nil value.

   Examples:
   ```clojure
   (coalesce nil nil 3 4)     ;; => 3
   (coalesce nil false 3)     ;; => false
   (coalesce nil nil nil)     ;; => nil
   ```"
  [& args]
  (first (remove nil? args)))

(s/fdef coalesce
  :args (s/cat :args (s/* any?))
  :ret any?)

(defn default-nil
  "Returns `x` if non-nil, otherwise returns `default`. Unlike `(or x default)`, this only checks
   for `nil`, not falsiness. This means `false` is preserved rather than replaced.

   Examples:
   ```clojure
   (default-nil 5 0)      ;; => 5
   (default-nil nil 0)    ;; => 0
   (default-nil false 0)  ;; => false (preserved, unlike `or`)
   ```"
  [x default]
  (if (nil? x) default x))

(s/fdef default-nil
  :args (s/cat :x any? :default any?)
  :ret any?)

;;;; Map utilities
(defn remove-nil-vals
  "Removes entries with `nil` values from a map. Returns an empty map if all values are `nil`.
   Preserves map type for hash-maps.

   Examples:
   ```clojure
   (remove-nil-vals {:a 1 :b nil :c 3})  ;; => {:a 1 :c 3}
   (remove-nil-vals {:a nil :b nil})     ;; => {}
   ```"
  [m]
  (into {} (remove (comp nil? val)) m))

(s/fdef remove-nil-vals
  :args (s/cat :m map?)
  :ret map?)

(defn replace-nil-vals
  "Replaces `nil` values in a map with `default`. All entries are preserved, but `nil` values are
   replaced.

   Examples:
   ```clojure
   (replace-nil-vals {:a 1 :b nil :c 3} 0)  ;; => {:a 1 :b 0 :c 3}
   (replace-nil-vals {:a nil :b nil} :missing)  ;; => {:a :missing :b :missing}
   ```"
  [m default]
  (into {} (map (fn [[k v]] [k (if (nil? v) default v)])) m))

(s/fdef replace-nil-vals
  :args (s/cat :m map? :default any?)
  :ret map?)
