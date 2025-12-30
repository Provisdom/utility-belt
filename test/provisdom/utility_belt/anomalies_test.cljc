(ns provisdom.utility-belt.anomalies-test
  #?(:cljs (:require-macros
             [provisdom.test.core]))
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;3 seconds

#?(:clj (set! *warn-on-reflection* true))

(t/deftest anomaly?-test
  (t/with-instrument `anomalies/anomaly?
    (t/is-spec-check anomalies/anomaly?))
  (t/with-instrument :all
    (t/is (anomalies/anomaly? {::anomalies/category ::anomalies/no-solve}))
    (t/is-not (anomalies/anomaly? {::anomalies/message "Test"}))))

(t/deftest chain-test
  ;; skip instrumentation - function arg causes issues
  ;; non-anomaly passes through
  (t/is= 6 (anomalies/chain 5 inc))
  ;; anomaly short-circuits
  (let [anomaly {::anomalies/category ::anomalies/error}]
    (t/is= anomaly (anomalies/chain anomaly inc)))
  ;; chaining multiple
  (t/is= 7 (-> 5
               (anomalies/chain inc)
               (anomalies/chain inc))))

(t/deftest chain->-test
  ;; skip instrumentation - function arg causes issues
  ;; with extra args
  (t/is= 8 (anomalies/chain-> 5 + 3))
  ;; anomaly short-circuits
  (let [anomaly {::anomalies/category ::anomalies/error}]
    (t/is= anomaly (anomalies/chain-> anomaly + 3))))

(t/deftest recover-test
  ;; skip instrumentation - function arg causes issues
  ;; non-anomaly passes through
  (t/is= 5 (anomalies/recover 5 (constantly 0)))
  ;; anomaly triggers recovery
  (t/is= 0 (anomalies/recover {::anomalies/category ::anomalies/not-found}
             (constantly 0)))
  ;; recovery fn receives anomaly
  (t/is= ::anomalies/error
    (anomalies/recover {::anomalies/category ::anomalies/error}
      ::anomalies/category)))

(t/deftest recover-with-test
  (t/with-instrument `anomalies/recover-with
    (t/is-spec-check anomalies/recover-with))
  (t/with-instrument :all
    ;; non-anomaly passes through
    (t/is= 5 (anomalies/recover-with 5 0))
    ;; anomaly uses fallback
    (t/is= [] (anomalies/recover-with {::anomalies/category ::anomalies/error} []))
    ;; nil fallback
    (t/is= nil (anomalies/recover-with {::anomalies/category ::anomalies/error} nil))))

#?(:clj
   (t/deftest anomaly-try-test
     (t/with-instrument :all
       ;; success case
       (t/is= 5 (anomalies/anomaly-try (+ 2 3)))
       ;; exception case
       (let [result (anomalies/anomaly-try (/ 1 0))]
         (t/is (anomalies/anomaly? result))
         (t/is= ::anomalies/exception (::anomalies/category result))))))
