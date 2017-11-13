(ns provisdom.utility-belt.async
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.core.async :as async]
    [provisdom.utility-belt.anomalies :as anomalies]))

(defn catch-error-or-nil
  "For a fn `f` that takes zero arguments, when Error or Exception is caught, returns false by default.
  Optionally, `default` can be a value,
  or a function that takes an Exception or Error and can return anything."
  ([f] (catch-error-or-nil f (fn [e] false)))
  ([f default]
   (try (let [r (f)]
          (if (or (nil? r)
                  (instance? Exception r))
            (throw (ex-info "nil result" {:fn (var catch-error-or-nil)}))
            r))
        (catch Exception e (if (fn? default)
                             (default e)
                             default))
        (catch Error e (if (fn? default)
                         (default e)
                         default)))))

(s/fdef catch-error-or-nil
        :args (s/cat :f (s/fspec :args (s/cat) :ret any?)
                     :default (s/? any?))
        :ret any?)

;;; TODO - has async monad written all over it
(defn thread
  "Call each of the functions `fs` on a separate thread.
   By default, each of the `fs` are wrapped in a default `wrap-fn` prevents hangs by
   catching Exceptions, Errors, and nil values, and returns false.
   Options for `threading-type`:
   :and -- Returns false if any are falsey, otherwise returns vector with all 
           the results in order corresponding to their `fs`.
        -- Short-circuits (and cancels the calls to remaining `fs`) on first
           falsey value returned.
   :first!! -- Returns nil if all are falsey, otherwise returns value of first 
               truthy result.
            -- Short-circuits (and cancels the calls to remaining `fs`) on first
               truthy value returned.
            -- This induces race conditions between threads and can lead to 
               inconsistent results. 
   :any-ordered -- Returns a tuple containing the [value index] as soon as any
                   function returns truthy with the previous `fs` all having returned
                   falsey.
   :all -- Returns a vector with all the results in order corresponding to 
           their `fs`.
   :or -- Returns true if any are truthy, otherwise returns false.
       -- Short-circuits (and cancels the calls to remaining `fs`) on first
          truthy value returned."
  ([threading-type fs] (thread threading-type fs catch-error-or-nil))
  ([threading-type fs wrap-fn]
   (let [futures (doall (for [f (map (fn [i g]
                                       [i #(wrap-fn g)])
                                     (range)
                                     fs)]
                          (let [c (async/chan)]
                            [(future (async/>!! c ((second f))))
                             c
                             (first f)])))]
     (loop [futures futures
            res (vec fs)]
       (if (seq futures)
         (let [[result c] (async/alts!! (map second futures) :priority true)
               i (peek (first (filter #(= c (second %)) futures)))
               res (assoc res i result)
               f-on #(remove (fn [e] (identical? (peek e) i)) futures)
               f-off #(doseq [fus (map first futures)]
                        (future-cancel fus))]
           (condp = threading-type
             :and (if result
                    (recur (f-on) res)
                    (do (f-off) false))
             :first!! (if-not result
                        (recur (f-on) res)
                        (do (f-off) result))
             :or (if-not result
                   (recur (f-on) res)
                   (do (f-off) true))
             :any-ordered (let [[first-truthy index] (reduce-kv (fn [tot i e]
                                                                  (cond (fn? e) (reduced [false -1])
                                                                        (not e) tot
                                                                        :else (reduced [e i])))
                                                                [nil -1]
                                                                res)]
                            (if-not first-truthy
                              (recur (f-on) res)
                              (do (f-off) [first-truthy index])))
             :all (recur (f-on) res)
             :else (throw (ex-info (str "Invalid option: " threading-type) {:fn (var thread)}))))
         (if (or (= threading-type :all)
                 (= threading-type :and))
           res
           false))))))

(s/fdef thread
        :args (s/cat :threading-type #{:and :first!! :or :any-ordered :all}
                     :fs (s/coll-of fn?)
                     :wrap-fn (s/? fn?))
        :ret any?)

(defn thread-select
  "Call each of the functions `fs` on a separate thread,
  and select thread result using `selector-fn`.
  By default, each of the `fs` are wrapped in a default `wrap-fn` prevents hangs by
  catching Exceptions, Errors, and nil values, and returns false."
  ([selector-fn fs] (thread-select selector-fn fs catch-error-or-nil))
  ([selector-fn fs wrap-fn] (let [res (filter #(not (not %))
                                              (thread :all fs wrap-fn))]
                              (when (first res)
                                (selector-fn res)))))

(s/fdef thread-select
        :args (s/cat :selector-fn fn?
                     :fs (s/coll-of fn?)
                     :wrap-fn (s/? fn?))
        :ret any?)

(defn thread-max
  "Call each of the functions `fs` on a separate thread, and returns the maximum value.
  Each of the `fs` are wrapped in a `wrap-fn` prevents hangs by
  catching Exceptions, Errors, and nil values, and returns false."
  [fs]
  (try (thread-select #(apply max %) fs)
       (catch Exception _ {::anomalies/category ::anomalies/forbidden
                           ::anomalies/message  "all functions fs must return numbers"
                           ::anomalies/fn       (var thread-max)})))

(s/fdef thread-max
        :args (s/cat :fs (s/coll-of fn?))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :max number?))

(defn thread-min
  "Call each of the functions `fs` on a separate thread, and returns the minimum value.
  Each of the `fs` are wrapped in a `wrap-fn` prevents hangs by
  catching Exceptions, Errors, and nil values, and returns false."
  [fs]
  (try (thread-select #(apply min %) fs)
       (catch Exception _ {::anomalies/category ::anomalies/forbidden
                           ::anomalies/message  "all functions fs must return numbers"
                           ::anomalies/fn       (var thread-min)})))

(s/fdef thread-min
        :args (s/cat :fs (s/coll-of fn?))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :min number?))