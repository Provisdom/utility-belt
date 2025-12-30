(ns provisdom.utility-belt.spec-ext-test
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.spec-ext :as spec-ext]))

;;1 second

(set! *warn-on-reflection* true)

;; test helper function for fdef tests
(defn test-fn [x] (inc x))

(defn throwing-test-fn [x]
  (if (neg? x)
    (throw (ArithmeticException. "negative!"))
    x))

(t/deftest get-meta-test
  (t/with-instrument `spec-ext/get-meta
    (t/is-spec-check spec-ext/get-meta))
  (t/with-instrument :all
    ;; nil for spec without metadata
    (s/def ::no-meta-spec int?)
    (t/is= nil (spec-ext/get-meta ::no-meta-spec))
    ;; returns metadata for spec with metadata
    (spec-ext/def ::with-meta-spec {:custom "value"} string?)
    (t/is= {:custom "value"} (spec-ext/get-meta ::with-meta-spec))
    ;; cleanup
    (spec-ext/clear-meta! ::with-meta-spec)))

(t/deftest specs-with-meta-test
  (t/with-instrument `spec-ext/specs-with-meta
    (t/is-spec-check spec-ext/specs-with-meta))
  (t/with-instrument :all
    ;; add some specs with metadata
    (spec-ext/def ::meta-spec-1 {:id 1} int?)
    (spec-ext/def ::meta-spec-2 {:id 2} string?)
    ;; specs-with-meta returns them
    (let [specs (set (spec-ext/specs-with-meta))]
      (t/is (contains? specs ::meta-spec-1))
      (t/is (contains? specs ::meta-spec-2)))
    ;; cleanup
    (spec-ext/clear-meta! ::meta-spec-1)
    (spec-ext/clear-meta! ::meta-spec-2)))

(t/deftest clear-meta!-test
  (t/with-instrument `spec-ext/clear-meta!
    (t/is-spec-check spec-ext/clear-meta!))
  (t/with-instrument :all
    ;; add metadata
    (spec-ext/def ::clear-test {:to-clear true} int?)
    (t/is= {:to-clear true} (spec-ext/get-meta ::clear-test))
    ;; clear returns previous metadata
    (t/is= {:to-clear true} (spec-ext/clear-meta! ::clear-test))
    ;; now nil
    (t/is= nil (spec-ext/get-meta ::clear-test))
    ;; clearing again returns nil
    (t/is= nil (spec-ext/clear-meta! ::clear-test))))

(t/deftest throwing-fdefs-test
  (t/with-instrument `spec-ext/throwing-fdefs
    (t/is-spec-check spec-ext/throwing-fdefs))
  (t/with-instrument :all
    ;; define fdef with throws
    (spec-ext/fdef throwing-test-fn
      :args (s/cat :x int?)
      :ret int?
      :throws [ArithmeticException])
    ;; throwing-fdefs includes it
    (let [fdefs (spec-ext/throwing-fdefs)]
      (t/is (contains? fdefs `throwing-test-fn))
      (t/is= [ArithmeticException] (get fdefs `throwing-test-fn)))))

(t/deftest def-test
  ;; def macro returns the spec key
  (t/with-instrument :all
    (let [result (spec-ext/def ::def-return-test int?)]
      (t/is= ::def-return-test result))
    ;; with docstring
    (let [result (spec-ext/def ::def-doc-test "A docstring" int?)]
      (t/is= ::def-doc-test result)
      (t/is= {:doc "A docstring"} (spec-ext/get-meta ::def-doc-test)))
    ;; with metamap
    (let [result (spec-ext/def ::def-meta-test {:custom "data"} int?)]
      (t/is= ::def-meta-test result)
      (t/is= {:custom "data"} (spec-ext/get-meta ::def-meta-test)))
    ;; with docstring and metamap
    (let [result (spec-ext/def ::def-both-test "Doc" {:extra "info"} int?)]
      (t/is= ::def-both-test result)
      (t/is= {:doc "Doc" :extra "info"} (spec-ext/get-meta ::def-both-test)))
    ;; redefining without metadata clears old metadata
    (spec-ext/def ::stale-meta-test {:old "metadata"} int?)
    (t/is= {:old "metadata"} (spec-ext/get-meta ::stale-meta-test))
    (spec-ext/def ::stale-meta-test int?)
    (t/is= nil (spec-ext/get-meta ::stale-meta-test))
    ;; cleanup
    (spec-ext/clear-meta! ::def-doc-test)
    (spec-ext/clear-meta! ::def-meta-test)
    (spec-ext/clear-meta! ::def-both-test)))

(t/deftest fdef-test
  ;; fdef macro returns the qualified symbol
  (t/with-instrument :all
    (let [result (spec-ext/fdef test-fn
                   :args (s/cat :x int?)
                   :ret int?)]
      (t/is= `test-fn result))
    ;; with throws stores metadata
    (spec-ext/fdef throwing-test-fn
      :args (s/cat :x int?)
      :ret int?
      :throws [ArithmeticException])
    (t/is= {:throws [ArithmeticException]}
      (spec-ext/get-meta `throwing-test-fn))
    ;; redefining without throws clears metadata
    (spec-ext/fdef throwing-test-fn
      :args (s/cat :x int?)
      :ret int?)
    (t/is= nil (spec-ext/get-meta `throwing-test-fn))
    ;; spec is properly registered
    (t/is (s/get-spec `test-fn))))
