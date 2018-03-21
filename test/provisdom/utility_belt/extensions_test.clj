(ns provisdom.utility-belt.extensions-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.extensions :as extensions]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;37 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

(deftest update-in-with-not-found-test
  (is (spec-check extensions/update-in-with-not-found
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 150}}))
  (is= {:a 3 :b 2}
       (extensions/update-in-with-not-found {:a 1 :b 2}
                                            [:a]
                                            (fn [v & args]
                                              (when (number? v)
                                                (+ v 2)))
                                            6))
  (is= {:a 1 :b 2 :c 8}
       (extensions/update-in-with-not-found {:a 1 :b 2}
                                            [:c]
                                            (fn [v & args]
                                              (when (number? v)
                                                (+ v 2)))
                                            6)))

(deftest interleave-all-test
  (is (spec-check extensions/interleave-all
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 100}})
      (is= '() (extensions/interleave-all)))
  (is= '() (extensions/interleave-all nil))
  (is= '(1) (extensions/interleave-all [1]))
  (is= '() (extensions/interleave-all nil nil))
  (is= '(1 2 3) (extensions/interleave-all [1 2 3] nil))
  (is= '(1 4 2 3) (extensions/interleave-all [1 2 3] [4]))
  (is= '(1 4 2 5 3 6 7 8) (extensions/interleave-all [1 2 3] [4 5 6 7 8]))
  (is= '(1 4 9 2 5 3 6 7 8)
       (extensions/interleave-all [1 2 3] [4 5 6 7 8] [9])))

(deftest reduce-kv-ext-test
  (is (spec-check extensions/reduce-kv-ext
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 200}}))
  (is= 9.0
       (extensions/reduce-kv-ext
         (fn [res i v1]
           (when (and (number? res)
                      (number? v1))
             (+ res i v1)))
         0.0
         [1 2 3]))
  (is= 24.0
       (extensions/reduce-kv-ext
         (fn [res i v1 v2]
           (when (and (number? res)
                      (number? v1)
                      (number? v2))
             (+ res i v1 v2)))
         0.0
         [1 2 3]
         [4 5 6]))
  (is= 48.0
       (extensions/reduce-kv-ext
         (fn [res i v1 v2 v3]
           (when (and (number? res)
                      (number? v1)
                      (number? v2)
                      (number? v3))
             (+ res i v1 v2 v3)))
         0.0
         [1 2 3]
         [4 5 6]
         [7 8 9])))

(deftest reductions-kv-test
  (is (spec-check extensions/reductions-kv
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 300}}))
  (is= '(1.0 4.0 9.0)
       (extensions/reductions-kv
         (fn [res i v1]
           (when (and (number? res)
                      (number? v1))
             (+ res i v1)))
         0.0
         [1 2 3]))
  (is= '(5.0 13.0 24.0)
       (extensions/reductions-kv
         (fn [res i v1 v2]
           (when (and (number? res)
                      (number? v1)
                      (number? v2))
             (+ res i v1 v2)))
         0.0
         [1 2 3]
         [4 5 6]))
  (is= '(12.0 28.0 48.0)
       (extensions/reductions-kv
         (fn [res i v1 v2 v3]
           (when (and (number? res)
                      (number? v1)
                      (number? v2)
                      (number? v3))
             (+ res i v1 v2 v3)))
         0.0
         [1 2 3]
         [4 5 6]
         [7 8 9])))

(deftest reduce-kv-with-stop-test
  (is (spec-check extensions/reduce-kv-with-stop
                  {:coll-check-limit 10
                   :coll-error-limit 10
                   :fspec-iterations 10
                   :recursion-limit  1
                   :test-check       {:num-tests 200}}))
  (is= 4.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1]
           (when (and (number? res)
                      (number? v1))
             (+ res i v1)))
         0.0
         [1 2 3]
         {::extensions/stop-pred1 (fn [res i v1]
                                    (> i 1))}))
  (is= 13.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1 v2]
           (when (and (number? res)
                      (number? v1)
                      (number? v2))
             (+ res i v1 v2)))
         0.0
         [1 2 3]
         [4 5 6]
         {::extensions/stop-pred2 (fn [res i v1 v2]
                                    (> i 1))}))
  (is= -6.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1]
           (when (and (number? res)
                      (number? v1))
             (+ res i v1)))
         0.0
         [1 2 3]
         {::extensions/stop-pred1     (fn [res i v1]
                                        (> i 1))
          ::extensions/err-pred1      (fn [res i v1]
                                        (> i 0))
          ::extensions/err-return-fn1 (fn [res i v1]
                                        -6.0)}))
  (is= -6.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1 v2]
           (when (and (number? res)
                      (number? v1)
                      (number? v2))
             (+ res i v1 v2)))
         0.0
         [1 2 3]
         [4 5 6]
         {::extensions/stop-pred2     (fn [res i v1 v2]
                                        (> i 1))
          ::extensions/err-pred2      (fn [res i v1 v2]
                                        (> i 0))
          ::extensions/err-return-fn2 (fn [res i v1 v2]
                                        -6.0)}))
  (is= -6.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1 v2 v3]
           (when (and (number? res)
                      (number? v1)
                      (number? v2)
                      (number? v3))
             (+ res i v1 v2 v3)))
         0.0
         [1 2 3]
         [4 5 6]
         [7 8 9]
         {::extensions/stop-pred3     (fn [res i v1 v2 v3]
                                        (> i 1))
          ::extensions/err-pred3      (fn [res i v1 v2 v3]
                                        (> i 0))
          ::extensions/err-return-fn3 (fn [res i v1 v2 v3]
                                        -6.0)}))
  (is= 12.0
       (extensions/reduce-kv-with-stop
         (fn [res i v1 v2 v3]
           (when (and (number? res)
                      (number? v1)
                      (number? v2)
                      (number? v3))
             (+ res i v1 v2 v3)))
         0.0
         [1 2 3]
         [4 5 6]
         [7 8 9]
         {::extensions/stop-pred3     (fn [res i v1 v2 v3]
                                        (> i 0))
          ::extensions/err-pred3      (fn [res i v1 v2 v3]
                                        (> i 1))
          ::extensions/err-return-fn3 (fn [res i v1 v2 v3]
                                        -6.0)})))

#_(ost/unstrument)
