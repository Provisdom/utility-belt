(ns provisdom.utility-belt.spec-ext
  "Extensions to `clojure.spec`.

   Provides:
   - [[def]] - Like `s/def` but supports metadata maps
   - [[fdef]] - Enhanced `s/fdef` with `:throws` clause support
   - [[get-meta]] - Retrieve stored metadata for a spec
   - [[specs-with-meta]] - List all specs that have metadata
   - [[clear-meta!]] - Remove metadata for a spec
   - [[throwing-fdefs]] - Find all fdefs that declare `:throws`"
  (:refer-clojure :exclude [def])
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::throws
  (s/coll-of (s/or
               ;; e.g., ArithmeticException
               :class class?
               ;; e.g., anomalies/anomaly-ex?
               :pred symbol?)))

(defonce ^{:doc "Internal atom storing spec metadata. Do not use directly."} *spec->meta (atom {}))

(s/def ::spec-name
  (s/or :keyword qualified-keyword?
        :symbol qualified-symbol?))

(defn get-meta
  "Returns the metadata map associated with spec `s`, or `nil` if none exists. Metadata is set
   via the [[def]] macro's metamap parameter or the [[fdef]] macro's `:throws` clause."
  [s]
  (get @*spec->meta s))

(s/fdef get-meta
  :args (s/cat :s ::spec-name)
  :ret (s/nilable map?))

(defn specs-with-meta
  "Returns a sequence of all spec keys that have metadata."
  []
  (or (keys @*spec->meta) []))

(s/fdef specs-with-meta
  :args (s/cat)
  :ret (s/coll-of ::spec-name))

(defn clear-meta!
  "Removes the metadata for spec `s`. Returns the previous metadata, or `nil`."
  [s]
  (let [prev (get @*spec->meta s)]
    (swap! *spec->meta dissoc s)
    prev))

(s/fdef clear-meta!
  :args (s/cat :s ::spec-name)
  :ret (s/nilable map?))

(defn throwing-fdefs
  "Returns a map of spec keys to their `:throws` values for all fdefs that declare throws."
  []
  (into {}
    (keep (fn [[k v]]
            (when-let [throws (:throws v)]
              [k throws])))
    @*spec->meta))

(s/fdef throwing-fdefs
  :args (s/cat)
  :ret (s/map-of qualified-symbol? ::throws))

(s/def ::def-args
  (s/cat
    :docstring (s/? string?)
    :metamap (s/? map?)
    :spec some?))

(defn- def-form
  [k arg-m]
  (when (s/invalid? arg-m)
    (throw (ex-info "Invalid call to def." {:explain (s/explain-data ::def-args arg-m)})))
  (let [{:keys [metamap docstring spec]} arg-m
        metamap (cond-> metamap
                  docstring (assoc :doc docstring))
        mm-sym (gensym "metamap")]
    `(let [~mm-sym ~metamap]
       (s/def ~k ~spec)
       ~(if metamap
          `(swap! *spec->meta assoc ~k ~mm-sym)
          `(swap! *spec->meta dissoc ~k))
       ~k)))

(defmacro def
  "Like `s/def` but supports setting a map of metadata."
  {:arglists '([name doc-string? metamap? spec])}
  [k & s-def-declarative]
  (def-form k (s/conform ::def-args s-def-declarative)))

(defmacro fdef
  "Takes a symbol naming a function, and one or more of the following:

   - `:args` A regex spec for the function arguments as they were a list to be passed to apply -
     in this way, a single spec can handle functions with multiple arities
   - `:ret` A spec for the function's return value
   - `:fn` A spec of the relationship between args and ret - the value passed is
     `{:args conformed-args :ret conformed-ret}` and is expected to contain predicates that relate
     those values
   - `:throws` A collection of exception classes or predicates that the function may throw

   Qualifies `fn-sym` with resolve, or using `*ns*` if no resolution found. Registers an fspec in
   the global registry, where it can be retrieved by calling `get-spec` with the var or
   fully-qualified symbol.

   Once registered, function specs are included in doc, checked by instrument, tested by the
   runner `clojure.spec.test.alpha/check`, and (if a macro) used to explain errors during
   macro-expansion.

   Note that `:fn` specs require the presence of `:args` and `:ret` specs to conform values, and
   so `:fn` specs will be ignored if `:args` or `:ret` are missing.

   Returns the qualified `fn-sym`.

   For example, to register function specs for the symbol function:
   ```clojure
   (s/fdef clojure.core/symbol
     :args (s/alt :separate (s/cat :ns string? :n string?)
                  :str string?
                  :sym symbol?)
     :ret symbol?)
   ```"
  [fn-sym & specs]
  (let [{:keys [args ret fn throws]} (apply hash-map specs)
        _ (s/assert (s/nilable ::throws) throws)
        fspec-opts (cond-> []
                     args (into [:args args])
                     ret (into [:ret ret])
                     fn (into [:fn fn]))
        fn-sym-qual-sym (gensym "fn-sym-qual")]
    `(let [~fn-sym-qual-sym (s/def ~fn-sym (s/fspec ~@fspec-opts))]
       ~(if throws
          `(swap! *spec->meta assoc-in [~fn-sym-qual-sym :throws] ~throws)
          `(swap! *spec->meta dissoc ~fn-sym-qual-sym))
       ~fn-sym-qual-sym)))
