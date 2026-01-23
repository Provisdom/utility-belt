/(ns provisdom.utility-belt.anomalies
  "Implements a standard anomaly (error) handling system based on Cognitect's approach.
   Provides functions and macros for working with anomalies as data rather than exceptions."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import (clojure.lang ExceptionInfo)))

(s/def ::category #{::error
                    ::exception
                    ::forbidden
                    ::incorrect
                    ::no-solve
                    ::not-found
                    ::third-party
                    ::unavailable
                    ::unsupported})

(s/def ::data any?)
(s/def ::ex-cause #?(:clj #(instance? Throwable %) :cljs #(instance? js/Error %)))
(s/def ::fn var?)
(s/def ::message string?)

(s/def ::anomaly
  (s/with-gen
    (s/keys :req [::category]
      :opt [::data
            ::fn
            ::message])
    #(gen/fmap (fn [[cat msg]]
                 (cond-> {::category cat}
                   msg (assoc ::message msg)))
       (gen/tuple (s/gen ::category)
         (gen/one-of [(gen/return nil)
                      (gen/string-alphanumeric)])))))

(defn anomaly?
  "Tests whether `x` is an anomaly.
   Note: Excludes sorted maps because `get` on a sorted-map with a comparator that can't compare
   key types (e.g., numeric keys vs keyword lookup) throws ClassCastException."
  [x]
  (and (map? x) (not (sorted? x)) (some? (get x ::category))))

(s/fdef anomaly?
  :args (s/cat :x any?)
  :ret boolean?)

(defn anomaly-ex?
  "Tests whether `x` is an anomaly exception. An anomaly exception is an exception with an anomaly
  in the ex-data."
  [x]
  (anomaly? (ex-data x)))

(s/fdef anomaly-ex?
  :args (s/cat :x any?)
  :ret boolean?)

(defn ex
  "Creates an exception from an anomaly.
   Takes an anomaly map and returns an ex-info exception with the anomaly as ex-data.
   The exception message will be the anomaly's ::message if present, otherwise a default message."
  [anomaly]
  (let [message (::message anomaly
                  (str "Anomaly '" (name (::category anomaly)) "' category"))
        data (dissoc anomaly ::message ::ex-cause)]
    (if-let [ex-cause (::ex-cause anomaly)]
      (ex-info message data ex-cause)
      (ex-info message data))))

(s/fdef ex
  :args (s/cat :anomaly ::anomaly)
  :ret #(instance? #?(:clj ExceptionInfo :cljs js/Error) %))

(defn ex!
  "Creates and throws an exception from an anomaly."
  [anomaly]
  (throw (ex anomaly)))

(s/fdef ex!
  :args (s/cat :anomaly ::anomaly))

(defn ex?!
  "Throws an exception if the input is an anomaly, otherwise returns the input unchanged."
  [maybe-anomaly]
  (if (anomaly? maybe-anomaly)
    (ex! maybe-anomaly)
    maybe-anomaly))

(s/fdef ex?!
  :args (s/cat :maybe-anomaly (s/or :anomaly ::anomaly :any any?))
  :ret any?)

(defn not-implemented-anomaly
  "Creates an anomaly to indicate that a function is not implemented.
   Returns a map with category ::unsupported and the function var that was called."
  [fn-var]
  {::category ::unsupported
   ::fn       fn-var
   ::message  "Not Implemented"})

(s/fdef not-implemented-anomaly
  :args (s/cat :fn-var var?)
  :ret ::anomaly)

#?(:clj (defmacro ^{:private true} assert-args
          [& pairs]
          `(do (when-not ~(first pairs)
                 (throw (IllegalArgumentException.
                          (str (first ~'&form) " requires " ~(second pairs)
                            " in " ~'*ns* ":" (:line (meta ~'&form))))))
             ~(let [more (nnext pairs)]
                (when more
                  (list* `assert-args more))))))

#?(:clj (defn- anomalous-let*
          [bindings body]
          (let [[[binding-form binding-expr] & bindings] bindings]
            `(let [value# ~binding-expr]
               (if (anomaly? value#)
                 value#
                 (let [~binding-form value#]
                   ~@(if (empty? bindings)
                       body
                       [(anomalous-let* bindings body)])))))))

#?(:clj
   (defmacro anomalous-let
     "A let-like binding form that short-circuits on anomalies.
     Similar to Clojure's core `let`, but if any binding expression returns an anomaly, the entire
     expression immediately returns that anomaly without evaluating subsequent bindings or the body.

     Example:
     ```clojure
     (anomalous-let [a (possibly-anomalous-fn)
                    b (another-fn a)]
       (do-something a b))
     ```
     If `possibly-anomalous-fn` returns an anomaly, it's immediately returned without evaluating
     `another-fn` or `do-something`."
     [bindings & body]
     (assert-args
       (vector? bindings) "a vector for its binding"
       (even? (count bindings)) "an even number of forms in binding vector")
     (anomalous-let* (partition 2 (destructure bindings)) body)))

#?(:clj
   (defmacro anomaly-try
     "Evaluates body and catches exceptions, converting them to anomalies. Returns the result of
     body if successful, or an anomaly with category ::exception if an exception is thrown.

     Parameters:
     - body: Expressions to evaluate

     Example:
     ```clojure
     (anomaly-try
       (/ 1 0))
     ;; => {::category ::exception ::message \"Divide by zero\" ::data {...}}
     ```"
     [& body]
     `(try
        ~@body
        (catch #?(:clj Throwable :cljs js/Error) e#
          {::category ::exception
           ::message  (.getMessage e#)
           ::data     {:exception e#}}))))

(defn chain
  "Chains anomaly-aware functions together. If `x` is an anomaly, returns it.
   Otherwise, applies `f` to `x`.

   This is the monadic bind operation for the anomaly monad.

   Parameters:
   - x: A value that may be an anomaly
   - f: A function to apply if x is not an anomaly

   Examples:
   ```clojure
   (chain 5 inc)
   ;; => 6

   (chain {::category ::error} inc)
   ;; => {::category ::error}

   (-> (get-data)
       (chain process)
       (chain save))
   ```"
  [x f]
  (if (anomaly? x)
    x
    (f x)))

(s/fdef chain
  :args (s/cat :x any?
          :f (s/fspec :args (s/cat :v any?) :ret any?))
  :ret any?)

(defn chain->
  "Threading-friendly version of chain for use with ->.
  Applies `f` with `args` to `x` if `x` is not an anomaly.

   Parameters:
   - x: A value that may be an anomaly
   - f: A function to apply
   - args: Additional arguments to pass to f after x

   Example:
   ```clojure
   (-> (get-data)
       (chain-> process opt1 opt2)
       (chain-> save))
   ```"
  [x f & args]
  (if (anomaly? x)
    x
    (apply f x args)))

(s/fdef chain->
  :args (s/cat :x any?
          :f ifn?
          :args (s/* any?))
  :ret any?)

(defn recover
  "Provides a fallback value if `x` is an anomaly.
   If `x` is an anomaly, applies `recovery-fn` to it to get a fallback value.
   Otherwise, returns `x` unchanged.

   Parameters:
   - x: A value that may be an anomaly
   - recovery-fn: A function that takes an anomaly and returns a fallback value

   Examples:
   ```clojure
   (recover 5 (constantly 0))
   ;; => 5

   (recover {::category ::not-found} (constantly []))
   ;; => []

   (recover {::category ::error ::data {:default 42}}
            (fn [a] (get-in a [::data :default])))
   ;; => 42
   ```"
  [x recovery-fn]
  (if (anomaly? x)
    (recovery-fn x)
    x))

(s/fdef recover
  :args (s/cat :x any?
          :recovery-fn (s/fspec :args (s/cat :anomaly ::anomaly) :ret any?))
  :ret any?)

(defn recover-with
  "Provides a constant fallback value if `x` is an anomaly.
   Simpler version of `recover` when the recovery doesn't need the anomaly.

   Parameters:
   - x: A value that may be an anomaly
   - fallback: The value to return if x is an anomaly

   Examples:
   ```clojure
   (recover-with (get-user) {:name \"Guest\"})
   ;; => user or {:name \"Guest\"} if get-user returned an anomaly
   ```"
  [x fallback]
  (if (anomaly? x)
    fallback
    x))

(s/fdef recover-with
  :args (s/cat :x any? :fallback any?)
  :ret any?)
