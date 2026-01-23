(ns provisdom.utility-belt.async
  "Advanced parallel processing utilities built on core.async. Provides high-level functions for
   executing tasks in parallel with various coordination patterns and error handling strategies."
  (:require
    [clojure.core.async :as async]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.utility-belt.anomalies :as anomalies]))

(s/def ::allow-nil? boolean?)
(s/def ::parallel? boolean?)
(s/def ::timeout-ms pos-int?)

(defn catch-error-or-exception
  "Safely executes a function, converting errors and exceptions to anomalies. Takes a zero-arity
   function and returns either its result or an appropriate anomaly. This is particularly useful
   for async operations where exceptions can get lost.

   The function handles two cases:
   1. If `f` throws an Exception, returns an `::anomalies/exception` anomaly
   2. If `f` throws an Error, returns an `::anomalies/error` anomaly

   Parameters:
   - `f`: A function taking no arguments

   Returns:
   - The result of calling `f` if successful
   - An anomaly map if an exception/error is thrown"
  [f]
  (try (let [r (f)]
         (if (instance? Exception r)
           (throw (ex-info (.getMessage ^Exception r) {}))
           r))
       (catch Exception e
         {::anomalies/category ::anomalies/exception
          ::anomalies/data     {:exception e}
          ::anomalies/fn       (var catch-error-or-exception)
          ::anomalies/message  (.getMessage e)})
       (catch Error e
         {::anomalies/category ::anomalies/error
          ::anomalies/data     {:exception e}
          ::anomalies/fn       (var catch-error-or-exception)
          ::anomalies/message  (.getMessage e)})))

(s/fdef catch-error-or-exception
  :args (s/cat :f (s/fspec :args (s/cat)
                    :ret any?))
  :ret any?)

(defn catch-error-or-exception-or-nil
  "Safely executes a function, converting errors, exceptions, and `nil` results to anomalies.
   Takes a zero-arity function and returns either its result or an appropriate anomaly. This is
   particularly useful for async operations where exceptions can get lost and `nil` values can
   cause blocked channels.

   The function handles three cases:
   1. If `f` returns `nil`, returns an `::anomalies/exception` anomaly
   2. If `f` throws an Exception, returns an `::anomalies/exception` anomaly
   3. If `f` throws an Error, returns an `::anomalies/error` anomaly

   Parameters:
   - `f`: A function taking no arguments

   Returns:
   - The result of calling `f` if successful and non-nil
   - An anomaly map otherwise

   See also: [[catch-error-or-exception]] if `nil` is a valid return value."
  [f]
  (let [r (catch-error-or-exception f)]
    (if (nil? r)
      {::anomalies/message  "'nil' return"
       ::anomalies/category ::anomalies/exception
       ::anomalies/fn       (var catch-error-or-exception-or-nil)}
      r)))

(s/fdef catch-error-or-exception-or-nil
  :args (s/cat :f (s/fspec :args (s/cat)
                    :ret any?))
  :ret any?)

(defn- make-timeout-anomaly
  "Creates a timeout anomaly."
  [timeout-ms]
  {::anomalies/message  (str "Timeout after " timeout-ms "ms")
   ::anomalies/category ::anomalies/unavailable
   ::anomalies/fn       (var make-timeout-anomaly)})

;; Wrapper to handle nil values through core.async channels (which close on nil)
(defrecord ValueWrapper [value])

(defn- wrap-value [v] (->ValueWrapper v))
(defn- unwrap-value [w] (:value w))

(defn thread
  "Call each of the functions `fs` on a separate thread. Each of the `fs` are wrapped to catch
   Exceptions and Errors, returning an anomaly instead.

   Options for `threading-type`:
   - `:all` -- Returns a vector with all the results in order corresponding to their `fs`. This is
     the recommended mode for most use cases.
   - `:and` -- Returns `nil` if any anomalies are returned, otherwise returns a vector with all
     the results in order corresponding to their `fs`. Short-circuits (and cancels the calls to
     remaining `fs`) on first returned anomaly.
   - `:or` -- Returns `false` if all are anomalies, otherwise returns `true`. Short-circuits (and
     cancels the calls to remaining `fs`) on first value returned.
   - `:first-success!!` -- Returns `nil` if all are anomalies, otherwise returns value of first
     successful result. Short-circuits (and cancels the calls to remaining `fs`) on first returned
     value. WARNING: Induces race conditions between threads.
   - `:race!!` -- Returns the first result regardless of success or anomaly. Short-circuits (and
     cancels the calls to remaining `fs`) on first returned result. WARNING: Induces race
     conditions between threads.
   - `:any-ordered` -- Returns a tuple containing the `[value index]` as soon as any function
     returns with the previous `fs` all having returned an anomaly. Otherwise, returns `nil`.

   Optional `opts` map:
   - `:timeout-ms` -- Maximum time in milliseconds to wait for all results. If exceeded, remaining
     functions are canceled and a timeout anomaly is returned for those slots.
   - `:allow-nil?` -- If `true`, `nil` returns are allowed. If `false` (default), `nil` returns
     are converted to anomalies.

   Examples:
   ```clojure
   (thread :all [#(compute-a) #(compute-b)])
   (thread :all [#(compute-a) #(compute-b)] {:timeout-ms 5000})
   (thread :first-success!! [#(try-method-a) #(try-method-b)])
   ```"
  ([threading-type fs] (thread threading-type fs {}))
  ([threading-type fs {:keys [allow-nil? timeout-ms] :or {allow-nil? false}}]
   (let [wrap-fn (if allow-nil?
                   catch-error-or-exception
                   catch-error-or-exception-or-nil)
         timeout-chan (when timeout-ms (async/timeout timeout-ms))
         futures (doall
                   (for [[index f] (map-indexed vector fs)]
                     (let [channel (async/chan)]
                       [(future (async/>!! channel (wrap-value (wrap-fn f))))
                        channel
                        index])))]
     (loop [futures futures
            results-and-fns (vec fs)
            timed-out? false]
       (if (and (seq futures) (not timed-out?))
         (let [channels (cond-> (mapv second futures)
                          timeout-chan (conj timeout-chan))
               [wrapped-result channel] (async/alts!! channels :priority true)]
           (if (and timeout-chan (= channel timeout-chan))
             ;; Timeout occurred
             (let [cancel-all #(doseq [fut (map first futures)]
                                 (future-cancel fut))
                   timeout-anomaly (make-timeout-anomaly timeout-ms)
                   ;; Fill remaining slots with timeout anomalies
                   final-results (reduce
                                   (fn [acc [_ _ idx]]
                                     (assoc acc idx timeout-anomaly))
                                   results-and-fns
                                   futures)]
               (cancel-all)
               (condp = threading-type
                 :all final-results
                 :and nil
                 :or false
                 :first-success!! nil
                 :race!! timeout-anomaly
                 :any-ordered nil))
             ;; Normal result - unwrap the value
             (let [latest-result (unwrap-value wrapped-result)
                   index (peek (first (filter #(= channel (second %)) futures)))
                   results-and-fns (assoc results-and-fns index latest-result)
                   remaining-futures #(remove (fn [e] (identical? (peek e) index))
                                        futures)
                   cancel-all #(doseq [fut (map first futures)]
                                 (future-cancel fut))]
               (condp = threading-type
                 :and (if (anomalies/anomaly? latest-result)
                        (do (cancel-all) nil)
                        (recur (remaining-futures) results-and-fns false))
                 :first-success!! (if (anomalies/anomaly? latest-result)
                                    (recur (remaining-futures) results-and-fns false)
                                    (do (cancel-all) latest-result))
                 :race!! (do (cancel-all) latest-result)
                 :or (if (anomalies/anomaly? latest-result)
                       (recur (remaining-futures) results-and-fns false)
                       (do (cancel-all) true))
                 :any-ordered (let [[first-val i] (reduce-kv
                                                    (fn [tot i e]
                                                      (cond (fn? e)
                                                            (reduced [nil -1])

                                                            (anomalies/anomaly? e)
                                                            tot

                                                            :else (reduced [e i])))
                                                    [nil -1]
                                                    results-and-fns)]
                                (if (= i -1)
                                  (recur (remaining-futures) results-and-fns false)
                                  (do (cancel-all) [first-val i])))
                 :all (recur (remaining-futures) results-and-fns false)))))
         ;; No more futures or timed out
         (when (#{:all :and} threading-type)
           results-and-fns))))))

(s/def ::threading-type #{:all :and :or :first-success!! :race!! :any-ordered})

(s/def ::thread-opts
  (s/keys :opt-un [::allow-nil? ::timeout-ms]))

(s/def ::fs
  (s/with-gen
    (s/coll-of (s/fspec :args (s/cat)
                 :ret any?))
    #(gen/vector (s/gen (s/fspec :args (s/cat)
                          :ret any?))
       0
       5)))

(s/fdef thread
  :args (s/cat :threading-type ::threading-type
          :fs ::fs
          :opts (s/? ::thread-opts))
  :ret any?)

(s/def ::min-successes pos-int?)
(s/def ::progress-fn (s/fspec :args (s/cat :completed nat-int?
                                      :total nat-int?
                                      :successes nat-int?)
                        :ret any?))

(s/def ::thread-select-opts
  (s/keys :opt-un [::allow-nil? ::min-successes ::parallel? ::progress-fn
                   ::timeout-ms]))

(defn thread-select
  "Executes functions and selects from their results using a custom selector function. Runs
   multiple functions (in parallel by default) and applies a selector function to the successful
   (non-anomaly) results. This provides a flexible way to choose from multiple computation results.

   Parameters:
   - `selector-fn`: A function that takes a collection of results and returns a single value
   - `fs`: A collection of zero-arity functions to execute
   - `opts`: (Optional) Map of options:
     - `:parallel?` - Run in parallel (default: `true`)
     - `:timeout-ms` - Maximum time to wait for results
     - `:min-successes` - Minimum successful results required before calling selector
       (short-circuits once reached)
     - `:progress-fn` - Called after each result: `(fn [completed total successes] ...)`
     - `:allow-nil?` - If `true`, `nil` returns are allowed (default: `false`)

   Returns the result of applying `selector-fn` to the collection of successful results.

   Examples:
   ```clojure
   ;; Find the maximum result from multiple calculations
   (thread-select #(apply max %) [#(calc1) #(calc2) #(calc3)])

   ;; Run sequentially instead of in parallel
   (thread-select first [#(slow-op) #(fallback)] {:parallel? false})

   ;; With timeout and minimum successes
   (thread-select #(apply max %)
                  [#(solver-a) #(solver-b) #(solver-c)]
                  {:timeout-ms 5000 :min-successes 2})

   ;; With progress tracking
   (thread-select best-result solvers
                  {:progress-fn (fn [done total ok]
                                  (println done \"/\" total \"complete,\" ok \"succeeded\"))})
   ```"
  ([selector-fn fs] (thread-select selector-fn fs {}))
  ([selector-fn fs opts]
   (let [{:keys [allow-nil? min-successes parallel? progress-fn timeout-ms]
          :or   {allow-nil? false parallel? true}} opts
         wrap-fn (if allow-nil?
                   catch-error-or-exception
                   catch-error-or-exception-or-nil)]
     (if parallel?
       ;; Parallel execution with optional early termination
       (let [timeout-chan (when timeout-ms (async/timeout timeout-ms))
             total (count fs)
             futures (doall
                       (for [[index f] (map-indexed vector fs)]
                         (let [channel (async/chan)]
                           [(future (async/>!! channel (wrap-value (wrap-fn f))))
                            channel
                            index])))]
         (loop [remaining-futures futures
                results []
                completed 0]
           (if (seq remaining-futures)
             (let [channels (cond-> (mapv second remaining-futures)
                              timeout-chan (conj timeout-chan))
                   [wrapped-result channel] (async/alts!! channels)]
               (if (and timeout-chan (= channel timeout-chan))
                 ;; Timeout - cancel remaining and return what we have
                 (do
                   (doseq [fut (map first remaining-futures)]
                     (future-cancel fut))
                   (selector-fn results))
                 ;; Normal result - unwrap the value
                 (let [result (unwrap-value wrapped-result)
                       new-completed (inc completed)
                       success? (not (anomalies/anomaly? result))
                       new-results (if success? (conj results result) results)
                       new-remaining (remove #(= channel (second %))
                                       remaining-futures)]
                   (when progress-fn
                     (progress-fn new-completed total (count new-results)))
                   ;; Check if we have enough successes to short-circuit
                   (if (and min-successes (>= (count new-results) min-successes))
                     (do
                       (doseq [fut (map first new-remaining)]
                         (future-cancel fut))
                       (selector-fn new-results))
                     (recur new-remaining new-results new-completed)))))
             ;; All done
             (selector-fn results))))
       ;; Sequential execution
       (let [total (count fs)]
         (loop [remaining-fs fs
                results []
                completed 0]
           (if (seq remaining-fs)
             (let [result (wrap-fn (first remaining-fs))
                   new-completed (inc completed)
                   success? (not (anomalies/anomaly? result))
                   new-results (if success? (conj results result) results)]
               (when progress-fn
                 (progress-fn new-completed total (count new-results)))
               (if (and min-successes (>= (count new-results) min-successes))
                 (selector-fn new-results)
                 (recur (rest remaining-fs) new-results new-completed)))
             (selector-fn results))))))))

(s/fdef thread-select
  :args (s/cat :selector-fn (s/fspec :args (s/cat :x (s/coll-of any?))
                              :ret any?)
          :fs ::fs
          :opts (s/? ::thread-select-opts))
  :ret any?)

