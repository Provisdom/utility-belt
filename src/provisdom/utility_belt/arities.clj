(ns provisdom.utility-belt.arities
  "Function introspection utilities for determining function arities.
   Provides tools to inspect and analyze function signatures at runtime
   using Java reflection."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import (java.lang.reflect Method)))

(s/def ::parameters (s/int-in 0 100))
(s/def ::variadic? boolean?)
(s/def ::arities (s/keys :req [::parameters ::variadic?]))

(defn arities
  "Uses reflection to analyze and return the arities of a function.
   
   Examines the Java class of the function to determine its fixed and variadic
   arities by analyzing the invoke and doInvoke methods.
   
   Parameters:
   - f: A Clojure function to analyze
   
   Returns:
   - A vector of maps, where each map contains:
     - ::parameters: The number of parameters (arity)
     - ::variadic?: Whether this arity is variadic (can take variable arguments)
   
   Example:
   ```clojure
   (arities +)
   ;; => [{::parameters 0, ::variadic? false}
   ;;     {::parameters 1, ::variadic? false}
   ;;     {::parameters 2, ::variadic? false}
   ;;     {::parameters 2, ::variadic? true}]
   
   (arities (fn [a b] (+ a b)))
   ;; => [{::parameters 2, ::variadic? false}]
   ```
   
   Note: Uses Java reflection, so works only in Clojure (JVM), not ClojureScript."
  [f]
  (let [all-declared-methods (.getDeclaredMethods (class f))
        methods-named-fn (fn [name]
                           (filter (fn [method]
                                     (= (.getName ^Method method) name))
                             all-declared-methods))
        methods-named-invoke (methods-named-fn "invoke")
        methods-named-do-invoke (methods-named-fn "doInvoke")
        invoked-params (sort (map
                               (fn [method]
                                 (alength (.getParameterTypes ^Method method)))
                               methods-named-invoke))
        invoked-params (mapv (fn [params]
                               {::parameters params
                                ::variadic?  false})
                         invoked-params)
        do-invoked-params (when-let [method (first methods-named-do-invoke)]
                            {::parameters (alength
                                            (.getParameterTypes ^Method method))
                             ::variadic?  true})
        results (if do-invoked-params
                  (conj invoked-params do-invoked-params)
                  invoked-params)]
    results))

(s/fdef arities
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of
                       (map
                         gen/return
                         (list (fn [a] a)
                           (fn [& a] (apply + a))
                           (fn [a b] (+ a b)))))))
  :ret (s/coll-of ::arities :kind vector? :into []))
