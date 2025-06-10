(ns provisdom.utility-belt.anomalies
  "Implements a standard anomaly (error) handling system based on Cognitect's approach.
   Provides functions and macros for working with anomalies as data rather than exceptions."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]))

;;;ANOMALIES
;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS-IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(s/def ::category #{::unavailable
                    ::interrupted
                    ::incorrect
                    ::forbidden
                    ::unsupported
                    ::not-found
                    ::conflict
                    ::fault
                    ::busy
                    ::third-party
                    ::no-solve
                    ::exception
                    ::error})

(s/def ::solver-category #{::bad-supplied-function})
(s/def ::message string?)
(s/def ::fn var?)
(s/def ::data any?)

(s/def ::anomaly
  (s/with-gen
    (s/keys :req [::category]
      :opt [::data
            ::fn
            ::message
            ::solver-category])
    #(gen/return {::category ::exception})))

(defn anomaly?
  "Tests whether `x` is an anomaly."
  [x]
  (and (map? x) (not (sorted? x)) (some? (get x ::category))))

(s/fdef anomaly?
  :args (s/cat :x any?)
  :ret boolean?)

(defn not-implemented-anomaly
  "Creates an anomaly to indicate that a function is not implemented.
   Returns a map with category ::unsupported and the function var that was called."
  [fn-var]
  {::category ::unsupported
   ::fn       fn-var
   ::message  "Not Implemented"})

#?(:clj (defmacro ^{:private true} assert-args
          [& pairs]
          `(do (when-not ~(first pairs)
                 (throw (IllegalArgumentException.
                          (str (first ~'&form) " requires " ~(second pairs)
                            " in " ~'*ns* ":" (:line (meta ~'&form))))))
             ~(let [more (nnext pairs)]
                (when more
                  (list* `assert-args more))))))

#?(:clj (defn- anomalous-let*
          [bindings body]
          (let [[[binding-form binding-expr] & bindings] bindings]
            `(let [value# ~binding-expr]
               (if (anomaly? value#)
                 value#
                 (let [~binding-form value#]
                   ~@(if (empty? bindings)
                       body
                       [(anomalous-let* bindings body)])))))))

#?(:clj
   (defmacro anomalous-let
     "A let-like binding form that short-circuits on anomalies.
     Similar to Clojure's core `let`, but if any binding expression 
     returns an anomaly, the entire expression immediately returns that anomaly
     without evaluating subsequent bindings or the body.
     
     Example:
     ```clojure
     (anomalous-let [a (possibly-anomalous-fn)
                    b (another-fn a)]
       (do-something a b))
     ```
     If `possibly-anomalous-fn` returns an anomaly, it's immediately returned
     without evaluating `another-fn` or `do-something`."
     [bindings & body]
     (assert-args
       (vector? bindings) "a vector for its binding"
       (even? (count bindings)) "an even number of forms in binding vector")
     (anomalous-let* (partition 2 (destructure bindings)) body)))
