(ns provisdom.utility-belt.arities
  "Function introspection utilities for determining function arities. Provides tools to inspect
   and analyze function signatures at runtime using Java reflection."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import (java.lang.reflect Method)))

(s/def ::parameters (s/int-in 0 21))
(s/def ::variadic? boolean?)
(s/def ::arities (s/keys :req [::parameters ::variadic?]))

(defn arities
  "Uses reflection to analyze and return the arities of a function. Examines the Java class of the
   function to determine its fixed and variadic arities by analyzing the `invoke` and `doInvoke`
   methods.

   Parameters:
   - `f`: A Clojure function to analyze

   Returns a vector of maps, where each map contains:
   - `::parameters`: The number of parameters (arity)
   - `::variadic?`: Whether this arity is variadic (can take variable arguments)

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
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list (fn [a] a)
                                                   (fn [& a] (apply + a))
                                                   (fn [a b] (+ a b)))))))
  :ret (s/coll-of ::arities :kind vector? :min-count 1))

(defn min-arity
  "Returns the minimum number of arguments the function accepts.

   Example:
   ```clojure
   (min-arity +)        ;; => 0
   (min-arity (fn [a b] (+ a b)))  ;; => 2
   ```"
  [f]
  (let [arity-info (arities f)
        effective-arity (fn [a]
                          (if (::variadic? a)
                            ;; For variadic, ::parameters includes rest-args param
                            (dec (::parameters a))
                            (::parameters a)))]
    (apply min (map effective-arity arity-info))))

(s/fdef min-arity
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list (fn [a] a)
                                                   (fn [& a] (apply + a))
                                                   (fn [a b] (+ a b)))))))
  :ret ::parameters)

(defn max-fixed-arity
  "Returns the maximum fixed (non-variadic) arity of the function. Returns `nil` if the function
   has no fixed arities (only variadic).

   Example:
   ```clojure
   (max-fixed-arity +)  ;; => 2
   (max-fixed-arity (fn [a b] (+ a b)))  ;; => 2
   (max-fixed-arity (fn [& args] args))  ;; => nil
   ```"
  [f]
  (let [arity-info (arities f)
        fixed-arities (filter (complement ::variadic?) arity-info)]
    (when (seq fixed-arities)
      (apply max (map ::parameters fixed-arities)))))

(s/fdef max-fixed-arity
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list (fn [a] a)
                                                   (fn [& a] (apply + a))
                                                   (fn [a b] (+ a b)))))))
  :ret (s/nilable ::parameters))

(defn variadic?
  "Returns `true` if the function accepts variable arguments.

   Example:
   ```clojure
   (variadic? +)        ;; => true
   (variadic? (fn [a b] (+ a b)))  ;; => false
   ```"
  [f]
  (let [arity-info (arities f)]
    (boolean (some ::variadic? arity-info))))

(s/fdef variadic?
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list (fn [a] a)
                                                   (fn [& a] (apply + a))
                                                   (fn [a b] (+ a b)))))))
  :ret boolean?)

(defn accepts-arity?
  "Returns `true` if the function can be called with `n` arguments.

   Example:
   ```clojure
   (accepts-arity? + 0)   ;; => true
   (accepts-arity? + 5)   ;; => true (variadic)
   (accepts-arity? (fn [a b] a) 2)  ;; => true
   (accepts-arity? (fn [a b] a) 3)  ;; => false
   ```"
  [f n]
  (let [arity-info (arities f)
        fixed-match (some #(and (not (::variadic? %)) (= (::parameters %) n)) arity-info)
        ;; For variadic, ::parameters includes the rest-args param,
        ;; so min Clojure arity is (::parameters - 1)
        variadic-match (some #(and (::variadic? %) (>= n (dec (::parameters %)))) arity-info)]
    (boolean (or fixed-match variadic-match))))

(s/fdef accepts-arity?
  :args (s/cat :f (s/with-gen fn?
                    #(gen/one-of (map gen/return (list (fn [a] a)
                                                   (fn [& a] (apply + a))
                                                   (fn [a b] (+ a b))))))
          :n nat-int?)
  :ret boolean?)
