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

(t/deftest anomaly-ex?-test
  (t/with-instrument `anomalies/anomaly-ex?
    (t/is-spec-check anomalies/anomaly-ex?))
  (t/with-instrument :all
    (let [anomaly {::anomalies/category ::anomalies/error}
          ex-anomaly (anomalies/ex anomaly)]
      (t/is (anomalies/anomaly-ex? ex-anomaly)))
    (t/is-not (anomalies/anomaly-ex? (ex-info "plain" {})))
    (t/is-not (anomalies/anomaly-ex? "not an exception"))
    (t/is-not (anomalies/anomaly-ex? nil))))

(t/deftest ex-test
  (t/with-instrument `anomalies/ex
    (t/is-spec-check anomalies/ex))
  (t/with-instrument :all
    ;; with message
    (let [anomaly {::anomalies/category ::anomalies/error
                   ::anomalies/message  "Test message"}
          result (anomalies/ex anomaly)]
      (t/is= "Test message" (ex-message result))
      (t/is= {::anomalies/category ::anomalies/error} (ex-data result)))
    ;; without message - uses default
    (let [anomaly {::anomalies/category ::anomalies/not-found}
          result (anomalies/ex anomaly)]
      (t/is= "Anomaly 'not-found' category" (ex-message result))
      (t/is= {::anomalies/category ::anomalies/not-found} (ex-data result)))
    ;; with cause
    #?(:clj
       (let [cause (Exception. "cause")
             anomaly {::anomalies/category ::anomalies/exception
                      ::anomalies/ex-cause cause}
             result (anomalies/ex anomaly)]
         (t/is= cause (ex-cause result))))))

(t/deftest ex!-test
  ;;spec-check would throw, so skip it
  (t/with-instrument :all
    (let [anomaly {::anomalies/category ::anomalies/error
                   ::anomalies/message  "Thrown"}]
      (t/is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
              #"Thrown"
              (anomalies/ex! anomaly))))))

(t/deftest ex?!-test
  ;;spec-check would throw, so skip it
  (t/with-instrument :all
    ;; non-anomaly passes through
    (t/is= 5 (anomalies/ex?! 5))
    (t/is= nil (anomalies/ex?! nil))
    (t/is= {:a 1} (anomalies/ex?! {:a 1}))
    ;; anomaly throws
    (let [anomaly {::anomalies/category ::anomalies/error
                   ::anomalies/message  "Should throw"}]
      (t/is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
              #"Should throw"
              (anomalies/ex?! anomaly))))))

(t/deftest not-implemented-anomaly-test
  ;;spec-check skipped; no built-in generator for var? predicate
  (t/with-instrument :all
    (let [result (anomalies/not-implemented-anomaly #'anomalies/anomaly?)]
      (t/is (anomalies/anomaly? result))
      (t/is= ::anomalies/unsupported (::anomalies/category result))
      (t/is= #'anomalies/anomaly? (::anomalies/fn result))
      (t/is= "Not Implemented" (::anomalies/message result)))))

#?(:clj
   (t/deftest anomalous-let-test
     ;;no instrumentation; macro
     ;; all bindings succeed
     (t/is= 6 (anomalies/anomalous-let [a 1
                                        b 2
                                        c 3]
                (+ a b c)))
     ;; first binding returns anomaly
     (let [anomaly {::anomalies/category ::anomalies/error}]
       (t/is= anomaly
         (anomalies/anomalous-let [a anomaly
                                   b (throw (Exception. "should not reach"))]
           (+ a b))))
     ;; middle binding returns anomaly
     (let [anomaly {::anomalies/category ::anomalies/not-found}]
       (t/is= anomaly
         (anomalies/anomalous-let [a 1
                                   b anomaly
                                   c (throw (Exception. "should not reach"))]
           (+ a b c))))
     ;; destructuring works
     (t/is= 3 (anomalies/anomalous-let [{:keys [x y]} {:x 1 :y 2}]
                (+ x y)))
     ;; destructuring with anomaly
     (let [anomaly {::anomalies/category ::anomalies/error}]
       (t/is= anomaly
         (anomalies/anomalous-let [{:keys [x]} anomaly]
           x)))))

#?(:clj
   (t/deftest anomaly-try-test
     (t/with-instrument :all
       ;; success case
       (t/is= 5 (anomalies/anomaly-try (+ 2 3)))
       ;; exception case
       (let [result (anomalies/anomaly-try (/ 1 0))]
         (t/is (anomalies/anomaly? result))
         (t/is= ::anomalies/exception (::anomalies/category result))))))

(t/deftest chain-test
  ;;no instrumentation; Orchestra fspec validation fails with fn args
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
  ;;no instrumentation; Orchestra fspec validation fails with fn args
  ;; with extra args
  (t/is= 8 (anomalies/chain-> 5 + 3))
  ;; anomaly short-circuits
  (let [anomaly {::anomalies/category ::anomalies/error}]
    (t/is= anomaly (anomalies/chain-> anomaly + 3))))

(t/deftest recover-test
  ;;no instrumentation; Orchestra fspec validation fails with fn args
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
