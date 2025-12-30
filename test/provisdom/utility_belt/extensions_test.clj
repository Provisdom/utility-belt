(ns provisdom.utility-belt.extensions-test
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.extensions :as ext]))

;22 seconds

(set! *warn-on-reflection* true)

(t/deftest update-in-with-not-found-test
  (t/with-instrument `ext/update-in-with-not-found
    (t/is-spec-check ext/update-in-with-not-found {:num-tests 150}))
  (t/with-instrument :all
    (t/is= {:a 3 :b 2}
      (ext/update-in-with-not-found {:a 1 :b 2}
        [:a]
        (fn [v & _args]
          (when (number? v)
            (+ v 2)))
        6))
    (t/is= {:a 1 :b 2 :c 8}
      (ext/update-in-with-not-found {:a 1 :b 2}
        [:c]
        (fn [v & _args]
          (when (number? v)
            (+ v 2)))
        6))))

(t/deftest interleave-all-test
  (t/with-instrument `ext/interleave-all
    (t/is-spec-check ext/interleave-all {:num-tests 150}))
  (t/with-instrument :all
    (t/is= '() (ext/interleave-all))
    (t/is= '() (ext/interleave-all nil))
    (t/is= '(1) (ext/interleave-all [1]))
    (t/is= '() (ext/interleave-all nil nil))
    (t/is= '(1 2 3) (ext/interleave-all [1 2 3] nil))
    (t/is= '(1 4 2 3) (ext/interleave-all [1 2 3] [4]))
    (t/is= '(1 4 2 5 3 6 7 8) (ext/interleave-all [1 2 3] [4 5 6 7 8]))
    (t/is= '(1 4 9 2 5 3 6 7 8) (ext/interleave-all [1 2 3] [4 5 6 7 8] [9]))))

(t/deftest reduce-kv-ext-test
  (t/with-instrument `ext/reduce-kv-ext
    (t/is-spec-check ext/reduce-kv-ext {:num-tests 150}))
  (t/with-instrument :all
    (t/is= 9.0
      (ext/reduce-kv-ext
        (fn [res i v1]
          (when (and (number? res) (number? v1))
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

(t/deftest reductions-kv-test
  (t/with-instrument `ext/reductions-kv
    (t/is-spec-check ext/reductions-kv {:num-tests 250}))
  (t/with-instrument :all
    (t/is= '(1.0 4.0 9.0)
      (ext/reductions-kv
        (fn [res i v1]
          (when (and (number? res) (number? v1))
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

(t/deftest reduce-kv-with-stop-test
  (t/with-instrument `ext/reduce-kv-with-stop
    (t/is-spec-check ext/reduce-kv-with-stop {:num-tests 130}))
  (t/with-instrument :all
    (t/is= 4.0
      (ext/reduce-kv-with-stop
        (fn [res i v1]
          (when (and (number? res) (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]
        {::ext/stop-pred1 (fn [_res i _v1]
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
        {::ext/stop-pred2 (fn [_res i _v1 _v2]
                            (> i 1))}))
    (t/is= -6.0
      (ext/reduce-kv-with-stop
        (fn [res i v1]
          (when (and (number? res) (number? v1))
            (+ res i v1)))
        0.0
        [1 2 3]
        {::ext/stop-pred1     (fn [_res i _v1]
                                (> i 1))
         ::ext/err-pred1      (fn [_res i _v1]
                                (> i 0))
         ::ext/err-return-fn1 (fn [_res _i _v1]
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
        {::ext/stop-pred2     (fn [_res i _v1 _v2]
                                (> i 1))
         ::ext/err-pred2      (fn [_res i _v1 _v2]
                                (> i 0))
         ::ext/err-return-fn2 (fn [_res i _v1 _v2]
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
        {::ext/stop-pred3     (fn [_res i _v1 _v2 _v3]
                                (> i 1))
         ::ext/err-pred3      (fn [_res i _v1 _v2 _v3]
                                (> i 0))
         ::ext/err-return-fn3 (fn [_res i _v1 _v2 _v3]
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
        {::ext/stop-pred3     (fn [_res i _v1 _v2 _v3]
                                (> i 0))
         ::ext/err-pred3      (fn [_res i _v1 _v2 _v3]
                                (> i 1))
         ::ext/err-return-fn3 (fn [_res i _v1 _v2 _v3]
                                -6.0)}))))

;;; New utility tests
(t/deftest deep-merge-test
  ;; skip instrumentation - merge-with calls deep-merge recursively with non-map values
  ;; empty case
  (t/is= {} (ext/deep-merge))
  ;; single map
  (t/is= {:a 1} (ext/deep-merge {:a 1}))
  ;; basic merge
  (t/is= {:a 1 :b 2} (ext/deep-merge {:a 1} {:b 2}))
  ;; nested merge
  (t/is= {:a {:b 1 :c 2}} (ext/deep-merge {:a {:b 1}} {:a {:c 2}}))
  ;; override non-map with map
  (t/is= {:a {:b 2}} (ext/deep-merge {:a 1} {:a {:b 2}}))
  ;; override map with non-map
  (t/is= {:a 1} (ext/deep-merge {:a {:b 2}} {:a 1}))
  ;; multiple maps
  (t/is= {:a {:b 1 :c 2 :d 3}} (ext/deep-merge {:a {:b 1}} {:a {:c 2}} {:a {:d 3}})))

(t/deftest index-by-test
  ;; skip instrumentation - function arg causes issues
  ;; basic usage
  (t/is= {1 {:id 1 :name "a"} 2 {:id 2 :name "b"}}
    (ext/index-by :id [{:id 1 :name "a"} {:id 2 :name "b"}]))
  ;; empty coll
  (t/is= {} (ext/index-by :id []))
  ;; function keyfn
  (t/is= {2 1 4 3} (ext/index-by inc [1 3])))

(t/deftest frequencies-by-test
  ;; skip instrumentation - function arg causes issues
  ;; basic usage
  (t/is= {:a 2 :b 1} (ext/frequencies-by :type [{:type :a} {:type :b} {:type :a}]))
  ;; using a function
  (t/is= {false 3 true 2} (ext/frequencies-by even? [1 2 3 4 5]))
  ;; empty coll
  (t/is= {} (ext/frequencies-by identity [])))

(t/deftest partition-map-test
  ;; skip instrumentation - function arg causes issues
  ;; basic even/odd split
  (t/is= [[2 4] [1 3 5]] (ext/partition-map even? [1 2 3 4 5]))
  ;; all match
  (t/is= [[2 4 6] []] (ext/partition-map even? [2 4 6]))
  ;; none match
  (t/is= [[] [1 3 5]] (ext/partition-map even? [1 3 5]))
  ;; empty coll
  (t/is= [[] []] (ext/partition-map even? [])))

(t/deftest find-first-test
  ;; skip instrumentation - function arg causes issues
  ;; found
  (t/is= 4 (ext/find-first even? [1 3 4 5 6]))
  ;; not found
  (t/is= nil (ext/find-first neg? [1 2 3]))
  ;; empty coll
  (t/is= nil (ext/find-first even? []))
  ;; first element
  (t/is= 2 (ext/find-first even? [2 3 4 5])))

(t/deftest if-some-let-test
  ;; if-some-let checks for nil only, allows false
  ;; false is valid
  (t/is= [false 1] (ext/if-some-let [a false b 1] [a b] :else))
  ;; nil triggers else
  (t/is= :else (ext/if-some-let [a nil b 1] [a b] :else))
  ;; all non-nil
  (t/is= [1 2 3] (ext/if-some-let [a 1 b 2 c 3] [a b c] :else))
  ;; second binding nil
  (t/is= :else (ext/if-some-let [a 1 b nil c 3] [a b c] :else))
  ;; default else is nil
  (t/is= nil (ext/if-some-let [a nil] :then))
  ;; single binding non-nil
  (t/is= :then (ext/if-some-let [a 1] :then)))

(t/deftest when-some-let-test
  ;; when-some-let checks for nil only, allows false
  ;; false is valid
  (t/is= [false 1] (ext/when-some-let [a false b 1] [a b]))
  ;; nil returns nil
  (t/is= nil (ext/when-some-let [a nil b 1] [a b]))
  ;; all non-nil
  (t/is= [1 2] (ext/when-some-let [a 1 b 2] [a b]))
  ;; compare with when-all-let which rejects false
  (t/is= nil (ext/when-all-let [a false b 1] [a b])))

(t/deftest dissoc-in-test
  (t/with-instrument `ext/dissoc-in
    (t/is-spec-check ext/dissoc-in))
  (t/with-instrument :all
    ;; basic nested dissoc
    (t/is= {:a {:c 2}} (ext/dissoc-in {:a {:b 1 :c 2}} [:a :b]))
    ;; removes empty parent
    (t/is= {} (ext/dissoc-in {:a {:b 1}} [:a :b]))
    ;; deep cleanup
    (t/is= {} (ext/dissoc-in {:a {:b {:c 1}}} [:a :b :c]))
    ;; preserves siblings
    (t/is= {:a {:b 1} :d 4} (ext/dissoc-in {:a {:b 1} :c 3 :d 4} [:c]))
    ;; path doesn't exist - returns unchanged
    (t/is= {:a 1} (ext/dissoc-in {:a 1} [:b :c]))
    ;; top-level dissoc
    (t/is= {:b 2} (ext/dissoc-in {:a 1 :b 2} [:a]))
    ;; empty map
    (t/is= {} (ext/dissoc-in {} [:a]))))

(t/deftest assoc-some-test
  (t/with-instrument `ext/assoc-some
    (t/is-spec-check ext/assoc-some {:num-tests 150}))
  (t/with-instrument :all
    ;; skips nil values
    (t/is= {:a 1 :b 2 :d 3} (ext/assoc-some {:a 1} :b 2 :c nil :d 3))
    ;; all nil
    (t/is= {:a 1} (ext/assoc-some {:a 1} :b nil :c nil))
    ;; no nils
    (t/is= {:a 1 :b 2 :c 3} (ext/assoc-some {:a 1} :b 2 :c 3))
    ;; empty base map
    (t/is= {:b 2} (ext/assoc-some {} :a nil :b 2))
    ;; false is not nil
    (t/is= {:a false} (ext/assoc-some {} :a false))
    ;; no kvs
    (t/is= {:a 1} (ext/assoc-some {:a 1}))))

(t/deftest distinct-by-test
  ;; skip instrumentation - function arg causes issues
  ;; basic by keyword
  (t/is= [{:id 1 :n "a"} {:id 2 :n "b"}]
    (vec (ext/distinct-by :id [{:id 1 :n "a"} {:id 2 :n "b"} {:id 1 :n "c"}])))
  ;; by first element
  (t/is= [[1 :a] [2 :b] [3 :d]]
    (vec (ext/distinct-by first [[1 :a] [2 :b] [1 :c] [3 :d]])))
  ;; empty coll
  (t/is= [] (vec (ext/distinct-by identity [])))
  ;; all unique
  (t/is= [1 2 3] (vec (ext/distinct-by identity [1 2 3])))
  ;; all same key
  (t/is= [{:k 1 :v :first}]
    (vec (ext/distinct-by :k [{:k 1 :v :first} {:k 1 :v :second} {:k 1 :v :third}])))
  ;; preserves order (first seen)
  (t/is= [:a :b :c] (vec (ext/distinct-by name [:a :b :c :a :b]))))

(t/deftest index-of-test
  ;; skip instrumentation - function arg causes issues
  ;; found in middle
  (t/is= 2 (ext/index-of even? [1 3 4 5 6]))
  ;; not found
  (t/is= nil (ext/index-of neg? [1 2 3]))
  ;; empty coll
  (t/is= nil (ext/index-of even? []))
  ;; first element matches
  (t/is= 0 (ext/index-of even? [2 3 4 5]))
  ;; last element matches
  (t/is= 4 (ext/index-of #(= % :target) [:a :b :c :d :target]))
  ;; equality predicate
  (t/is= 2 (ext/index-of #(= % "c") ["a" "b" "c" "d"])))

(t/deftest safe-nth-test
  (t/with-instrument `ext/safe-nth
    (t/is-spec-check ext/safe-nth))
  (t/with-instrument :all
    ;; valid index
    (t/is= 2 (ext/safe-nth [1 2 3] 1))
    ;; first element
    (t/is= 1 (ext/safe-nth [1 2 3] 0))
    ;; last element
    (t/is= 3 (ext/safe-nth [1 2 3] 2))
    ;; out of bounds - default nil
    (t/is= nil (ext/safe-nth [1 2 3] 10))
    ;; out of bounds - custom default
    (t/is= :missing (ext/safe-nth [1 2 3] 10 :missing))
    ;; negative index
    (t/is= nil (ext/safe-nth [1 2 3] -1))
    ;; negative with default
    (t/is= :neg (ext/safe-nth [1 2 3] -1 :neg))
    ;; empty coll
    (t/is= nil (ext/safe-nth [] 0))
    ;; works with lists
    (t/is= 2 (ext/safe-nth '(1 2 3) 1))))

(t/deftest keep-kv-test
  ;; skip instrumentation - function arg causes issues
  ;; keep even indices
  (t/is= '(:a :c :e) (ext/keep-kv (fn [i x] (when (even? i) x)) [:a :b :c :d :e]))
  ;; keep with index
  (t/is= '([1 2] [3 4])
    (ext/keep-kv (fn [i x] (when (> x 0) [i x])) [-1 2 -3 4]))
  ;; all nil results
  (t/is= '() (ext/keep-kv (fn [_i _x] nil) [1 2 3]))
  ;; empty coll
  (t/is= '() (ext/keep-kv (fn [i x] x) []))
  ;; transform values
  (t/is= '(0 2 6) (ext/keep-kv (fn [i x] (* i x)) [1 2 3])))

(t/deftest take-until-test
  ;; skip instrumentation - function arg causes issues
  ;; basic - includes terminating element
  (t/is= '(1 2 3 5) (ext/take-until #(< % 5) [1 2 3 5 6 7]))
  ;; compare with take-while - excludes terminating element
  (t/is= '(1 2 3) (take-while #(< % 5) [1 2 3 5 6 7]))
  ;; all pass pred
  (t/is= '(1 2 3) (ext/take-until #(< % 10) [1 2 3]))
  ;; first fails pred
  (t/is= '(5) (ext/take-until #(< % 5) [5 6 7]))
  ;; empty coll
  (t/is= '() (ext/take-until even? []))
  ;; single element passes
  (t/is= '(1 2) (ext/take-until odd? [1 2 3])))

(t/deftest drop-until-test
  ;; skip instrumentation - function arg causes issues
  ;; basic - starts from first failing element
  (t/is= '(5 6 7) (ext/drop-until #(< % 5) [1 2 3 5 6 7]))
  ;; compare with drop-while - same result in this case
  (t/is= '(5 6 7) (drop-while #(< % 5) [1 2 3 5 6 7]))
  ;; all pass pred - empty result
  (t/is= '() (ext/drop-until #(< % 10) [1 2 3]))
  ;; first fails pred - returns all
  (t/is= '(5 6 7) (ext/drop-until #(< % 5) [5 6 7]))
  ;; empty coll
  (t/is= '() (ext/drop-until even? []))
  ;; none pass
  (t/is= '(1 2 3) (ext/drop-until #(> % 10) [1 2 3])))
