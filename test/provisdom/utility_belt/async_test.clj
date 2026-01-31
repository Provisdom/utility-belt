(ns provisdom.utility-belt.async-test
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.async :as async]))

;;15 seconds

(set! *warn-on-reflection* true)

(t/deftest catch-error-or-exception-test
  (t/with-instrument `async/catch-error-or-exception
    (t/is-spec-check async/catch-error-or-exception))
  (t/with-instrument :all
    (t/is (async/catch-error-or-exception (constantly true)))
    (t/is= nil (async/catch-error-or-exception (constantly nil)))
    ;; when function returns an Exception, it's rethrown and caught
    (let [result (async/catch-error-or-exception (constantly (Exception. "HI")))]
      (t/is= "HI" (::anomalies/message result))
      (t/is= ::anomalies/exception (::anomalies/category result))
      (t/is= (var async/catch-error-or-exception) (::anomalies/fn result))
      ;; exception is preserved in ::data for stack trace access
      (t/is (instance? Exception (get-in result [::anomalies/data :exception]))))))

(t/deftest catch-error-or-exception-or-nil-test
  (t/with-instrument `async/catch-error-or-exception-or-nil
    (t/is-spec-check async/catch-error-or-exception-or-nil))
  (t/with-instrument :all
    (t/is (async/catch-error-or-exception-or-nil (constantly true)))
    ;; when function returns an Exception, it's rethrown and caught
    (let [result (async/catch-error-or-exception-or-nil (constantly (Exception. "HI")))]
      (t/is= "HI" (::anomalies/message result))
      (t/is= ::anomalies/exception (::anomalies/category result))
      (t/is= (var async/catch-error-or-exception) (::anomalies/fn result))
      (t/is (instance? Exception (get-in result [::anomalies/data :exception]))))
    ;; nil return becomes anomaly (no ::data since no exception thrown)
    (t/is= {::anomalies/category ::anomalies/exception
            ::anomalies/fn       (var async/catch-error-or-exception-or-nil)
            ::anomalies/message  "'nil' return"}
      (async/catch-error-or-exception-or-nil (constantly nil)))))

(t/deftest thread-test
  (t/with-instrument `async/thread
    (t/is-spec-check async/thread))
  (t/with-instrument :all
    ;; :and mode
    (t/is= [] (async/thread :and []))
    (t/is= nil
      (async/thread :and [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (t/is= [2 1] (async/thread :and [(constantly 2) (constantly 1)]))

    ;; :first-success!! mode (renamed from :first!!)
    (t/is= nil
      (async/thread :first-success!! [(constantly {::anomalies/category
                                                   ::anomalies/exception})]))
    (t/is= nil (async/thread :first-success!! []))

    ;; :race!! mode (new)
    (t/is= 42 (async/thread :race!! [(constantly 42)]))
    (t/is= {::anomalies/category ::anomalies/exception}
      (async/thread :race!! [(constantly {::anomalies/category
                                          ::anomalies/exception})]))
    (t/is= nil (async/thread :race!! []))

    ;; :or mode
    (t/is (async/thread :or [(constantly 2)
                             (constantly 1)
                             (constantly {::anomalies/category
                                          ::anomalies/exception})]))
    (t/is-not (async/thread :or [(constantly {::anomalies/category
                                              ::anomalies/exception})]))
    (t/is-not (async/thread :or []))

    ;; :any-ordered mode
    (t/is= [2 0]
      (async/thread :any-ordered
        [(constantly 2)
         (constantly 1)
         (constantly {::anomalies/category
                      ::anomalies/exception})]))
    (t/is= [2 1]
      (async/thread :any-ordered
        [(constantly {::anomalies/category ::anomalies/exception})
         (constantly 2)
         (constantly 1)]))
    (t/is= nil
      (async/thread :any-ordered
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))
    (t/is= nil (async/thread :any-ordered []))

    ;; :all mode
    (t/is= [2 1 {::anomalies/category ::anomalies/exception}]
      (async/thread :all [(constantly 2)
                          (constantly 1)
                          (constantly {::anomalies/category
                                       ::anomalies/exception})]))
    (t/is= [] (async/thread :all []))))

(t/deftest thread-allow-nil-test
  (t/with-instrument :all
    ;; Default behavior: nil becomes anomaly
    (let [result (async/thread :all [(constantly nil) (constantly 1)])]
      (t/is (anomalies/anomaly? (first result)))
      (t/is= 1 (second result)))
    ;; With allow-nil?: nil passes through
    (t/is= [nil 1]
      (async/thread :all [(constantly nil) (constantly 1)] {:allow-nil? true}))))

(t/deftest thread-timeout-test
  ;; Fast functions complete before timeout
  (t/is= [1 2]
    (async/thread :all [(constantly 1) (constantly 2)] {:timeout-ms 1000}))
  ;; Slow function times out
  (let [result (async/thread :all
                 [#(do (Thread/sleep 2000) 1)
                  (constantly 2)]
                 {:timeout-ms 100})]
    (t/is= 2 (count result))
    ;; One should be timeout anomaly, one should be 2
    (t/is (some #(and (anomalies/anomaly? %)
                   (= ::anomalies/unavailable (::anomalies/category %)))
            result))
    (t/is (some #(= 2 %) result))))

(t/deftest thread-select-test
  (t/with-instrument `async/thread-select
    (t/is-spec-check async/thread-select {:num-tests 150}))
  (t/with-instrument :all
    ;; Results may come back in any order in parallel mode
    (t/is= #{4 2}
      (set (async/thread-select (fn [results]
                                  (when (and (seq results)
                                          (every? number? results))
                                    (mapv (partial * 2) results)))
             [(constantly 2)
              (constantly 1)
              (constantly {::anomalies/category
                           ::anomalies/exception})])))
    (t/is= nil
      (async/thread-select (fn [results]
                             (when (and (seq results)
                                     (every? number? results))
                               (mapv (partial * 2) results)))
        [(constantly {::anomalies/category
                      ::anomalies/exception})]))))

(t/deftest thread-select-opts-test
  ;; Test :parallel? false - sequential execution preserves order
  (t/is= #{2 4}
    (set (async/thread-select (fn [results] (mapv #(* 2 %) results))
           [(constantly 1) (constantly 2)]
           {:parallel? false})))

  ;; Test :min-successes - should short-circuit after 2 successes
  (let [call-count (atom 0)
        result (async/thread-select
                 identity
                 [(fn [] (swap! call-count inc) 1)
                  (fn [] (swap! call-count inc) 2)
                  (fn [] (Thread/sleep 500) (swap! call-count inc) 3)]
                 {:min-successes 2
                  :parallel? false})]
    (t/is= 2 (count result))
    (t/is= 2 @call-count))

  ;; Test :progress-fn
  (let [progress-log (atom [])]
    (async/thread-select
      identity
      [(constantly 1) (constantly 2) (constantly 3)]
      {:parallel? false
       :progress-fn (fn [done total ok]
                      (swap! progress-log conj [done total ok]))})
    (t/is= [[1 3 1] [2 3 2] [3 3 3]] @progress-log)))

(t/deftest thread-select-timeout-test
  ;; Timeout returns partial results
  (let [result (async/thread-select
                 identity
                 [#(do (Thread/sleep 2000) 1)
                  (constantly 2)
                  (constantly 3)]
                 {:timeout-ms 200})]
    ;; Should have 2-3 results (the fast ones)
    (t/is (>= (count result) 2))
    (t/is (every? number? result))))

