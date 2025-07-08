(ns provisdom.utility-belt.spec-ext
  "Extensions to clojure.spec."
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::throws
  (s/coll-of (s/or
               ;; e.g., ArithmeticException
               :class class?
               ;; e.g., anomalies/anomaly-ex?
               :pred symbol?)))

(def _*fdef-throws (atom {}))

(defmacro fdef
  "Takes a symbol naming a function, and one or more of the following:

  :args A regex spec for the function arguments as they were a list to be
    passed to apply - in this way, a single spec can handle functions with
    multiple arities
  :ret A spec for the function's return value
  :fn A spec of the relationship between args and ret - the
    value passed is {:args conformed-args :ret conformed-ret} and is
    expected to contain predicates that relate those values
  :throws A collection of exception classes or predicates that the function
    may throw

  Qualifies fn-sym with resolve, or using *ns* if no resolution found.
  Registers an fspec in the global registry, where it can be retrieved
  by calling get-spec with the var or fully-qualified symbol.

  Once registered, function specs are included in doc, checked by
  instrument, tested by the runner clojure.spec.test.alpha/check, and (if
  a macro) used to explain errors during macroexpansion.

  Note that :fn specs require the presence of :args and :ret specs to
  conform values, and so :fn specs will be ignored if :args or :ret
  are missing.

  Returns the qualified fn-sym.

  For example, to register function specs for the symbol function:

  (s/fdef clojure.core/symbol
    :args (s/alt :separate (s/cat :ns string? :n string?)
                 :str string?
                 :sym symbol?)
    :ret symbol?)"
  [fn-sym & specs]
  (let [{:keys [throws]} specs
        _ (s/assert (s/nilable ::throws) throws)]
    `(let [~'fn-sym-qual (s/def ~fn-sym (s/fspec ~@specs))]
       ~@(when throws [`(swap! _*fdef-throws assoc ~'fn-sym-qual ~throws)])
       ~'fn-sym-qual)))
