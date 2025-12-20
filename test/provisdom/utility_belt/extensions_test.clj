(ns provisdom.utility-belt.extensions-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.extensions :as ext]))

;36 seconds

(set! *warn-on-reflection* true)

(ct/deftest update-in-with-not-found-test
  (t/with-instrument `ext/update-in-with-not-found
    (t/is-spec-check ext/update-in-with-not-found
          {:fspec-iterations 3})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= {:a 3 :b 2}
      (ext/update-in-with-not-found {:a 1 :b 2}
        [:a]
        (fn [v & args]
          (when (number? v)
            (+ v 2)))
        6))
    (t/is= {:a 1 :b 2 :c 8}
      (ext/update-in-with-not-found {:a 1 :b 2}
        [:c]
        (fn [v & args]
          (when (number? v)
            (+ v 2)))
        6))))

(ct/deftest interleave-all-test
  (t/with-instrument `ext/interleave-all
    (t/is-spec-check ext/interleave-all
          {:fspec-iterations 1
           :num-tests        150})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= '() (ext/interleave-all)))
  (t/is= '() (ext/interleave-all nil))
  (t/is= '(1) (ext/interleave-all [1]))
  (t/is= '() (ext/interleave-all nil nil))
  (t/is= '(1 2 3) (ext/interleave-all [1 2 3] nil))
  (t/is= '(1 4 2 3) (ext/interleave-all [1 2 3] [4]))
  (t/is= '(1 4 2 5 3 6 7 8) (ext/interleave-all [1 2 3] [4 5 6 7 8]))
  (t/is= '(1 4 9 2 5 3 6 7 8)
    (ext/interleave-all [1 2 3] [4 5 6 7 8] [9])))

(ct/deftest reduce-kv-ext-test
  (t/with-instrument `ext/reduce-kv-ext
    (t/is-spec-check ext/reduce-kv-ext
          {:fspec-iterations 5})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 9.0
      (ext/reduce-kv-ext
        (fn [res i v1]
          (when (and (number? res)
                  (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]))
    (t/is= 24.0
      (ext/reduce-kv-ext
        (fn [res i v1 v2]
          (when (and (number? res)
                  (number? v1)
                  (number? v2))
            (+ res i v1 v2)))
        0.0
        [1 2 3]
        [4 5 6]))
    (t/is= 48.0
      (ext/reduce-kv-ext
        (fn [res i v1 v2 v3]
          (when (and (number? res)
                  (number? v1)
                  (number? v2)
                  (number? v3))
            (+ res i v1 v2 v3)))
        0.0
        [1 2 3]
        [4 5 6]
        [7 8 9]))))

(ct/deftest reductions-kv-test
  (t/with-instrument `ext/reductions-kv
    (t/is-spec-check ext/reductions-kv
          {:fspec-iterations 5})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= '(1.0 4.0 9.0)
      (ext/reductions-kv
        (fn [res i v1]
          (when (and (number? res)
                  (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]))
    (t/is= '(5.0 13.0 24.0)
      (ext/reductions-kv
        (fn [res i v1 v2]
          (when (and (number? res)
                  (number? v1)
                  (number? v2))
            (+ res i v1 v2)))
        0.0
        [1 2 3]
        [4 5 6]))
    (t/is= '(12.0 28.0 48.0)
      (ext/reductions-kv
        (fn [res i v1 v2 v3]
          (when (and (number? res)
                  (number? v1)
                  (number? v2)
                  (number? v3))
            (+ res i v1 v2 v3)))
        0.0
        [1 2 3]
        [4 5 6]
        [7 8 9]))))

(ct/deftest reduce-kv-with-stop-test
  (t/with-instrument `ext/reduce-kv-with-stop
    (t/is-spec-check ext/reduce-kv-with-stop
          {:fspec-iterations 5})))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 4.0
      (ext/reduce-kv-with-stop
        (fn [res i v1]
          (when (and (number? res)
                  (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]
        {::ext/stop-pred1 (fn [res i v1]
                            (> i 1))}))
    (t/is= 13.0
      (ext/reduce-kv-with-stop
        (fn [res i v1 v2]
          (when (and (number? res)
                  (number? v1)
                  (number? v2))
            (+ res i v1 v2)))
        0.0
        [1 2 3]
        [4 5 6]
        {::ext/stop-pred2 (fn [res i v1 v2]
                            (> i 1))}))
    (t/is= -6.0
      (ext/reduce-kv-with-stop
        (fn [res i v1]
          (when (and (number? res)
                  (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]
        {::ext/stop-pred1     (fn [res i v1]
                                (> i 1))
         ::ext/err-pred1      (fn [res i v1]
                                (> i 0))
         ::ext/err-return-fn1 (fn [res i v1]
                                -6.0)}))
    (t/is= -6.0
      (ext/reduce-kv-with-stop
        (fn [res i v1 v2]
          (when (and (number? res)
                  (number? v1)
                  (number? v2))
            (+ res i v1 v2)))
        0.0
        [1 2 3]
        [4 5 6]
        {::ext/stop-pred2     (fn [res i v1 v2]
                                (> i 1))
         ::ext/err-pred2      (fn [res i v1 v2]
                                (> i 0))
         ::ext/err-return-fn2 (fn [res i v1 v2]
                                -6.0)}))
    (t/is= -6.0
      (ext/reduce-kv-with-stop
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
        {::ext/stop-pred3     (fn [res i v1 v2 v3]
                                (> i 1))
         ::ext/err-pred3      (fn [res i v1 v2 v3]
                                (> i 0))
         ::ext/err-return-fn3 (fn [res i v1 v2 v3]
                                -6.0)}))
    (t/is= 12.0
      (ext/reduce-kv-with-stop
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
        {::ext/stop-pred3     (fn [res i v1 v2 v3]
                                (> i 0))
         ::ext/err-pred3      (fn [res i v1 v2 v3]
                                (> i 1))
         ::ext/err-return-fn3 (fn [res i v1 v2 v3]
                                -6.0)}))))
