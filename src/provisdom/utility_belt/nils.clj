(ns provisdom.utility-belt.nils
  "Utilities for handling nil values in different ways.
   Provides functions for ignoring, reporting, or replacing nil values
   when applying functions or processing collections."
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.utility-belt.anomalies :as anomalies]))

(defn ignore-nils
  "Applies function `f` and ignores any nil values in `args`."
  [f & args]
  (apply f (remove nil? args)))

(s/fdef ignore-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn anomaly-nils
  "Applies function `f` but returns an anomaly if any argument is nil or if the result is nil.
   
   This function is useful for workflows where nil values should be treated as errors.
   Uses the anomalies system to report these nil-related errors in a standard way.
   
   Parameters:
   - f: The function to apply
   - args: Arguments to pass to the function
   
   Returns:
   - If any arg is nil: Returns a ::forbidden anomaly
   - If f returns nil: Returns a ::forbidden anomaly
   - Otherwise: Returns the result of (apply f args)
   
   Examples:
   ```clojure
   (anomaly-nils + 1 2 3)    ;; => 6
   (anomaly-nils + 1 nil 3)  ;; => {::anomalies/category ::anomalies/forbidden ...}
   (anomaly-nils (constantly nil) 1)  ;; => {::anomalies/category ::anomalies/forbidden ...}
   ```"
  [f & args]
  (let [anomaly {::anomalies/category ::anomalies/forbidden
                 ::anomalies/message  "nil not allowed"
                 ::anomalies/fn       (var anomaly-nils)}]
    (if (some nil? args)
      anomaly
      (or (apply f args) anomaly))))

(s/fdef anomaly-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn nil-nils
  "Applies function `f` but returns nil if any argument is nil.
   
   A simpler alternative to anomaly-nils when you don't need the detailed
   error reporting of anomalies. Can be used with threading macros to
   short-circuit on nil values.
   
   Parameters:
   - f: The function to apply
   - args: Arguments to pass to the function
   
   Returns:
   - If any arg is nil: Returns nil
   - Otherwise: Returns the result of (apply f args)
   
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
  "Returns a lazy sequence constructed by replacing nil values with values from a replacement collection.
   
   For each nil value found in the input collection, takes the next value from the 
   replacement collection. Non-nil values from the input collection are preserved.
   
   Parameters:
   - coll: The input collection that may contain nil values
   - replacement-coll: Collection of values to use in place of nils
   
   Returns:
   - A lazy sequence with nil values replaced by values from replacement-coll
   
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
