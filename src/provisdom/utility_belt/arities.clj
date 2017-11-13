(ns provisdom.utility-belt.arities
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost])
  (:import (java.lang.reflect Method)))

(s/def ::parameters (s/int-in 0 100))
(s/def ::variadic? boolean?)
(s/def ::arities (s/keys :req [::parameters ::variadic?]))

(defn arities
  "Uses reflection to return the arity numbers of the function `f`.
  Returns a vector with each element being a map of ::parameters and ::variadic?."
  [f]
  (let [all-declared-methods (.getDeclaredMethods (class f))
        methods-named-fn (fn [name]
                           (filter (fn [method]
                                     (= (.getName ^Method method) name))
                                   all-declared-methods))
        methods-named-invoke (methods-named-fn "invoke")
        methods-named-do-invoke (methods-named-fn "doInvoke")
        invoked-params (sort (map (fn [method]
                                    (alength (.getParameterTypes ^Method method)))
                                  methods-named-invoke))
        invoked-params (mapv (fn [params]
                               {::parameters params
                                ::variadic?  false})
                             invoked-params)
        do-invoked-params (when-let [method (first methods-named-do-invoke)]
                            {::parameters (alength (.getParameterTypes ^Method method))
                             ::variadic?  true})
        results (if do-invoked-params
                  (conj invoked-params do-invoked-params)
                  invoked-params)]
    results))

(s/fdef arities
        :args (s/cat :f fn?)
        :ret (s/coll-of ::arities :kind vector? :into []))