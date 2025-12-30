(ns provisdom.utility-belt.nils-test
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.nils :as nils]))

;21 seconds

(set! *warn-on-reflection* true)

(t/deftest ignore-nils-test
  (t/with-instrument `nils/ignore-nils
    (t/is-spec-check nils/ignore-nils {:num-tests 150}))
  (t/with-instrument :all
    (t/is-not (nils/ignore-nils (fn [& args]
                                  (when (and (= 1 (count args))
                                             (number? (first args)))
                                    (< (first args) 2)))
                                nil 4 nil))
    (t/is (nils/ignore-nils (fn [& args]
                              (when (and (= 1 (count args))
                                         (number? (first args)))
                                (< (first args) 2)))
                            nil 1 nil))
    (t/is (nils/ignore-nils (fn [& args]
                              (when (and (= 1 (count args))
                                         (number? (first args)))
                                (< (first args) 2)))
                            1))))

(t/deftest anomaly-nils-test
  (t/with-instrument `nils/anomaly-nils
    (t/is-spec-check nils/anomaly-nils {:num-tests 150}))
  (t/with-instrument :all
    (t/is= {::anomalies/category ::anomalies/forbidden
            ::anomalies/message  "nil not allowed"
            ::anomalies/fn       (var nils/anomaly-nils)}
           (nils/anomaly-nils (fn [& args]
                                (when (and (= 1 (count args))
                                           (number? (first args)))
                                  (< (first args) 2)))
                              nil 4 nil))
    (t/is= {::anomalies/category ::anomalies/forbidden
            ::anomalies/message  "nil not allowed"
            ::anomalies/fn       (var nils/anomaly-nils)}
           (nils/anomaly-nils (fn [& args]
                                (when (and (= 1 (count args))
                                           (number? (first args)))
                                  (< (first args) 2)))
                              4))
    (t/is (nils/anomaly-nils (fn [& args]
                               (when (and (= 1 (count args))
                                          (number? (first args)))
                                 (< (first args) 2)))
                             1))))

(t/deftest nil-nils-test
  (t/with-instrument `nils/nil-nils
    (t/is-spec-check nils/nil-nils {:num-tests 150}))
  (t/with-instrument :all
    (t/is= nil
           (nils/nil-nils (fn [& args]
                            (when (and (= 1 (count args))
                                       (number? (first args)))
                              (< (first args) 2)))
                          nil 4 nil))
    (t/is-not (nils/nil-nils (fn [& args]
                               (when (and (= 1 (count args))
                                          (number? (first args)))
                                 (< (first args) 2)))
                             4))))

(t/deftest replace-nils-test
  (t/with-instrument `nils/replace-nils
    (t/is-spec-check nils/replace-nils))
  (t/with-instrument :all
    (t/is= [1 2 3 4 5] (nils/replace-nils [1 2 3 nil 5] [4 nil 5]))
    (t/is= [1 2 4 nil 5] (nils/replace-nils [1 2 nil nil nil] [4 nil 5]))
    (t/is= [1 2 4 5 nil] (nils/replace-nils [1 2 nil nil nil] [4 5]))))

(t/deftest ignore-nils-fn-test
  ;; skip spec-check: fn? has no default generator
  (t/with-instrument :all
    (t/is= 6 ((nils/ignore-nils-fn +) 1 nil 2 nil 3))
    (t/is= 6 ((nils/ignore-nils-fn +) 1 2 3))
    (t/is= 0 ((nils/ignore-nils-fn +)))
    (t/is= 0 ((nils/ignore-nils-fn +) nil nil nil))
    ;; works with max
    (t/is= 5 ((nils/ignore-nils-fn max) 1 nil 5 nil 3))))

(t/deftest nil-nils-fn-test
  ;; skip spec-check: fn? has no default generator
  (t/with-instrument :all
    (t/is= 6 ((nils/nil-nils-fn +) 1 2 3))
    (t/is= nil ((nils/nil-nils-fn +) 1 nil 3))
    (t/is= nil ((nils/nil-nils-fn +) nil))
    ;; no args returns result of (f)
    (t/is= 0 ((nils/nil-nils-fn +)))))

(t/deftest anomaly-nils-fn-test
  ;; skip spec-check: fn? has no default generator
  (t/with-instrument :all
    (t/is= 6 ((nils/anomaly-nils-fn +) 1 2 3))
    (t/is= {::anomalies/category ::anomalies/forbidden
            ::anomalies/message  "nil not allowed"
            ::anomalies/fn       (var nils/anomaly-nils-fn)}
           ((nils/anomaly-nils-fn +) 1 nil 3))
    ;; returns anomaly when function returns nil
    (t/is= {::anomalies/category ::anomalies/forbidden
            ::anomalies/message  "nil not allowed"
            ::anomalies/fn       (var nils/anomaly-nils-fn)}
           ((nils/anomaly-nils-fn (constantly nil)) 1 2 3))))

(t/deftest coalesce-test
  (t/with-instrument `nils/coalesce
    (t/is-spec-check nils/coalesce))
  (t/with-instrument :all
    (t/is= 3 (nils/coalesce nil nil 3 4))
    (t/is= 1 (nils/coalesce 1 2 3))
    (t/is= nil (nils/coalesce nil nil nil))
    (t/is= nil (nils/coalesce))
    ;; false is preserved (unlike or)
    (t/is= false (nils/coalesce nil false 3))
    (t/is= false (nils/coalesce false nil 3))))

(t/deftest default-nil-test
  (t/with-instrument `nils/default-nil
    (t/is-spec-check nils/default-nil))
  (t/with-instrument :all
    (t/is= 5 (nils/default-nil 5 0))
    (t/is= 0 (nils/default-nil nil 0))
    ;; false is preserved (unlike or)
    (t/is= false (nils/default-nil false 0))
    ;; default can be any value
    (t/is= :missing (nils/default-nil nil :missing))
    (t/is= "hello" (nils/default-nil "hello" "default"))))

(t/deftest remove-nil-vals-test
  (t/with-instrument `nils/remove-nil-vals
    (t/is-spec-check nils/remove-nil-vals))
  (t/with-instrument :all
    (t/is= {:a 1 :c 3} (nils/remove-nil-vals {:a 1 :b nil :c 3}))
    (t/is= {} (nils/remove-nil-vals {:a nil :b nil}))
    (t/is= {} (nils/remove-nil-vals {}))
    ;; false values are preserved
    (t/is= {:a false :c 3} (nils/remove-nil-vals {:a false :b nil :c 3}))))

(t/deftest replace-nil-vals-test
  (t/with-instrument `nils/replace-nil-vals
    (t/is-spec-check nils/replace-nil-vals))
  (t/with-instrument :all
    (t/is= {:a 1 :b 0 :c 3} (nils/replace-nil-vals {:a 1 :b nil :c 3} 0))
    (t/is= {:a :missing :b :missing} (nils/replace-nil-vals {:a nil :b nil} :missing))
    (t/is= {} (nils/replace-nil-vals {} 0))
    ;; false values are preserved
    (t/is= {:a false :b 0 :c 3} (nils/replace-nil-vals {:a false :b nil :c 3} 0))))
