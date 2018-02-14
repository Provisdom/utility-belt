(ns provisdom.utility-belt.async
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [clojure.core.async :as async]
    [provisdom.utility-belt.anomalies :as anomalies]))

(defn catch-error-or-exception
  "For a fn `f` that takes zero arguments, when Error or Exception is caught,
  returns anomaly."
  [f]
  (try (let [r (f)]
         (if (instance? Exception r)
           (throw (ex-info (.getMessage r) {}))
           r))
       (catch Exception e {::anomalies/message  (.getMessage e)
                           ::anomalies/category ::anomalies/exception
                           ::anomalies/fn       (var catch-error-or-exception)})
       (catch Error e {::anomalies/message  (.getMessage e)
                       ::anomalies/category ::anomalies/error
                       ::anomalies/fn       (var catch-error-or-exception)})))

(s/fdef catch-error-or-exception
        :args (s/cat :f (s/fspec :args (s/cat)
                                 :ret any?))
        :ret any?)

;;; TODO - has async monad written all over it
(defn thread
  "Call each of the functions `fs` on a separate thread. Each of the `fs` are
  wrapped such that hangs are prevented by catching Exceptions and Errors, and
  returning an anomaly.

  Options for `threading-type`:
   `:and` -- Returns false if any anomalies are returned, otherwise returns a
             vector with all the results in order corresponding to their `fs`.
          -- Short-circuits (and cancels the calls to remaining `fs`) on first
             returned anomaly.
   `:first!!` -- Returns false if all are anomalies, otherwise returns value of
                 first result.
              -- Short-circuits (and cancels the calls to remaining `fs`) on
                 first returned value.
              -- This induces race conditions between threads and can lead to
                 inconsistent results.
   `:any-ordered` -- Returns a tuple containing the [value index] as soon as any
                     function returns with the previous `fs` all having
                     returned an anomaly. Otherwise, returns false.
   `:all` -- Returns a vector with all the results in order corresponding to
             their `fs`.
   `:or` -- Returns false if all are anomalies, otherwise returns true.
         -- Short-circuits (and cancels the calls to remaining `fs`) on first
            value returned."
  [threading-type fs]
  (let [futures (doall
                  (for [index-and-wrap-f (map (fn [index f]
                                                [index #(catch-error-or-exception f)])
                                              (range)
                                              fs)]
                    (let [channel (async/chan)]
                      [(future (async/>!! channel ((second index-and-wrap-f))))
                       channel
                       (first index-and-wrap-f)])))]
    (loop [futures futures
           results-and-fns (vec fs)]
      (if (seq futures)
        (let [[latest-result channel] (async/alts!! (map second futures) :priority true)
              index (peek (first (filter #(= channel (second %)) futures)))
              results-and-fns (assoc results-and-fns index latest-result)
              f-on #(remove (fn [e]
                              (identical? (peek e) index))
                            futures)
              f-off #(doseq [fus (map first futures)]
                       (future-cancel fus))]
          (condp = threading-type
            :and (if (anomalies/anomaly? latest-result)
                   (do (f-off) false)
                   (recur (f-on) results-and-fns))
            :first!! (if (anomalies/anomaly? latest-result)
                       (recur (f-on) results-and-fns)
                       (do (f-off) latest-result))
            :or (if (anomalies/anomaly? latest-result)
                  (recur (f-on) results-and-fns)
                  (do (f-off) true))
            :any-ordered (let [[first-val i] (reduce-kv
                                               (fn [tot i e]
                                                 (cond (fn? e) (reduced [nil -1])
                                                       (anomalies/anomaly? e) tot
                                                       :else (reduced [e i])))
                                               [nil -1]
                                               results-and-fns)]
                           (if (= i -1)
                             (recur (f-on) results-and-fns)
                             (do (f-off) [first-val i])))
            :all (recur (f-on) results-and-fns)))
        (if (or (= threading-type :all)
                (= threading-type :and))
          results-and-fns
          false)))))

(s/fdef thread
        :args (s/cat :threading-type #{:and :first!! :or :any-ordered :all}
                     :fs (s/coll-of fn?))
        :ret any?)

(defn thread-select
  "Call each of the functions `fs` on a separate thread, and select thread
  result using `selector-fn`. Each of the `fs` are wrapped such that hangs are
  prevented by catching Exceptions and Errors, and anomalies are ignored."
  [selector-fn fs]
  (let [result (filter (complement anomalies/anomaly?)
                       (thread :all fs))]
    (selector-fn result)))

(s/fdef thread-select
        :args (s/cat :selector-fn fn?
                     :fs (s/coll-of fn?))
        :ret any?)

(defn thread-max
  "Calls each of the functions `fs` on a separate thread, and returns the
  maximum value. Each of the `fs` are wrapped such that hangs are prevented by
  catching Exceptions and Errors, and anomalies are ignored."
  [fs]
  (try (thread-select #(apply max %) fs)
       (catch Exception _ {::anomalies/category ::anomalies/forbidden
                           ::anomalies/message  "All functions 'fs' must return numbers."
                           ::anomalies/fn       (var thread-max)})))

(s/fdef thread-max
        :args (s/cat :fs (s/coll-of fn?))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :max number?))

(defn thread-min
  "Calls each of the functions `fs` on a separate thread, and returns the
  minimum value. Each of the `fs` are wrapped such that hangs are prevented by
  catching Exceptions and Errors, and anomalies are ignored."
  [fs]
  (try (thread-select #(apply min %) fs)
       (catch Exception _ {::anomalies/category ::anomalies/forbidden
                           ::anomalies/message  "All functions 'fs' must return numbers."
                           ::anomalies/fn       (var thread-min)})))

(s/fdef thread-min
        :args (s/cat :fs (s/coll-of fn?))
        :ret (s/or :anomaly ::anomalies/anomaly
                   :min number?))