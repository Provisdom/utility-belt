(ns provisdom.utility-belt.arities-test
  (:require
    [provisdom.test.core :as t]
    [provisdom.utility-belt.arities :as arities]))

;1 second

(set! *warn-on-reflection* true)

(defn hi
  ([] "")
  ([_x] "")
  ([_x _y] "")
  ([_x _y & _z] ""))

(defn single-arity [a b] (+ a b))

(defn variadic-only [& args] (apply + args))

(t/deftest arities-test
  (t/with-instrument `arities/arities
    (t/is-spec-check arities/arities))
  (t/with-instrument :all
    ;; multi-arity with variadic
    (t/is= [{::arities/parameters 0
             ::arities/variadic?  false}
            {::arities/parameters 1
             ::arities/variadic?  false}
            {::arities/parameters 2
             ::arities/variadic?  false}
            {::arities/parameters 3
             ::arities/variadic?  true}]
      (arities/arities hi))
    ;; single fixed arity
    (t/is= [{::arities/parameters 2
             ::arities/variadic?  false}]
      (arities/arities single-arity))
    ;; variadic-only (doInvoke takes 1 param: the rest-args ISeq)
    (t/is= [{::arities/parameters 1
             ::arities/variadic?  true}]
      (arities/arities variadic-only))
    ;; core function (+ has 0, 1, 2 fixed arities and variadic from 2+)
    (t/is= [{::arities/parameters 0
             ::arities/variadic?  false}
            {::arities/parameters 1
             ::arities/variadic?  false}
            {::arities/parameters 2
             ::arities/variadic?  false}
            {::arities/parameters 3
             ::arities/variadic?  true}]
      (arities/arities +))))

(t/deftest min-arity-test
  (t/with-instrument `arities/min-arity
    (t/is-spec-check arities/min-arity))
  (t/with-instrument :all
    (t/is= 0 (arities/min-arity hi))
    (t/is= 2 (arities/min-arity single-arity))
    (t/is= 0 (arities/min-arity variadic-only))
    (t/is= 0 (arities/min-arity +))))

(t/deftest max-fixed-arity-test
  (t/with-instrument `arities/max-fixed-arity
    (t/is-spec-check arities/max-fixed-arity))
  (t/with-instrument :all
    (t/is= 2 (arities/max-fixed-arity hi))
    (t/is= 2 (arities/max-fixed-arity single-arity))
    (t/is= nil (arities/max-fixed-arity variadic-only))
    (t/is= 2 (arities/max-fixed-arity +))))

(t/deftest variadic?-test
  (t/with-instrument `arities/variadic?
    (t/is-spec-check arities/variadic?))
  (t/with-instrument :all
    (t/is (arities/variadic? hi))
    (t/is-not (arities/variadic? single-arity))
    (t/is (arities/variadic? variadic-only))
    (t/is (arities/variadic? +))))

(t/deftest accepts-arity?-test
  (t/with-instrument `arities/accepts-arity?
    (t/is-spec-check arities/accepts-arity?))
  (t/with-instrument :all
    ;; hi: 0, 1, 2, or 3+ args
    (t/is (arities/accepts-arity? hi 0))
    (t/is (arities/accepts-arity? hi 1))
    (t/is (arities/accepts-arity? hi 2))
    (t/is (arities/accepts-arity? hi 3))
    (t/is (arities/accepts-arity? hi 100))
    ;; single-arity: only 2 args
    (t/is-not (arities/accepts-arity? single-arity 0))
    (t/is-not (arities/accepts-arity? single-arity 1))
    (t/is (arities/accepts-arity? single-arity 2))
    (t/is-not (arities/accepts-arity? single-arity 3))
    ;; variadic-only: 0+ args
    (t/is (arities/accepts-arity? variadic-only 0))
    (t/is (arities/accepts-arity? variadic-only 5))
    ;; +: 0, 1, 2, or 2+ args
    (t/is (arities/accepts-arity? + 0))
    (t/is (arities/accepts-arity? + 1))
    (t/is (arities/accepts-arity? + 2))
    (t/is (arities/accepts-arity? + 10))))
