(ns provisdom.utility-belt.maps
  "Extended map utilities for working with priority maps, sorted maps, and map manipulation.
   Provides predicates, spec generators, and functions for creating and working with
   specialized map types with ordering guarantees.

   Note: For basic map-over-keys/values operations, use Clojure 1.11+ core functions:
   - `update-vals` - map function over values
   - `update-keys` - map function over keys"
  (:require
    [clojure.data.priority-map :refer [priority-map-keyfn-by]]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import (clojure.data.priority_map PersistentPriorityMap)
           (clojure.lang PersistentTreeMap)
           (java.util Map)))

(declare priority-map? priority-map)

(s/def ::priority-map
  (s/with-gen priority-map?
    #(gen/one-of [(priority-map identity < 1 2 3 4)])))

(s/def ::comparator (s/fspec :args (s/cat :k1 any? :k2 any?)
                      :ret int?))

;;;PRIORITY MAP
(defn priority-map?
  [m]
  (instance? PersistentPriorityMap m))

(s/fdef priority-map?
  :args (s/cat :m any?)
  :ret boolean?)

(defmacro priority-map-of
  "This macro builds the spec for a priority map built with [[priority-map]].
  `keyfn` is the function to apply to the VALUE of the map for comparison.
  `comparator` is how to compare the outputs of the `keyfn`. A common example
  would be to use 'identity' for the `keyfn` and '<' for the `comparator`."
  [kpred vpred keyfn comparator & opts]
  (let [sform `(s/map-of ~kpred ~vpred ~@opts)
        xform `(s/and ~sform priority-map?)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (priority-map-keyfn-by ~keyfn ~comparator))
          (s/gen ~sform)))))

(defn priority-map
  "Creates a [[clojure.data.priority-map/priority-map-keyfn-by]]. A common
  example would be to use 'identity' for the `keyfn` and '<' for the
  `comparator`."
  [keyfn comparator & keyvals]
  (apply priority-map-keyfn-by keyfn comparator keyvals))

(comment "documentation only"
  (s/fdef priority-map
    :args (s/cat :keyfn (s/fspec :args (s/cat :value any?)
                          :ret any?)
            :comparator ::comparator
            :keyvals (s/* any?))
    :ret boolean?))

;;;SORTED MAP
(defn sorted-map?
  [m]
  (instance? PersistentTreeMap m))

(s/fdef sorted-map?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-by?
  "Tests whether `m` is a sorted map by."
  [comparator m]
  (and (sorted-map? m)
    (= (keys m) (sort comparator (keys m)))))

(comment "documentation only"
  (s/fdef sorted-map-by?
    :args (s/cat :comparator ::comparator
            :m any?)
    :ret boolean?))

(defmacro sorted-map-of
  "This macro builds the spec for a sorted map."
  ([kpred vpred & opts]
   (let [sform `(s/map-of ~kpred ~vpred ~@opts)
         xform `(s/and sorted-map? ~sform)]
     `(s/with-gen
        ~xform
        #(gen/fmap (partial into (sorted-map))
           (s/gen ~sform))))))

(defmacro sorted-map-by-of
  "This macro builds the spec for a sorted map by."
  [kpred vpred comparator & opts]
  (let [sform `(s/map-of ~kpred ~vpred ~@opts)
        xform `(s/and (partial sorted-map-by? ~comparator) ~sform)]
    `(s/with-gen
       ~xform
       #(gen/fmap (partial into (sorted-map-by ~comparator))
          (s/gen ~sform)))))

;;;SORTED MAP MONOTONIC
(defn sorted-map-monotonic?
  "Tests whether `m` is a 'sorted map monotonic'. A 'sorted map monotonic' is a
  sorted map (by keys) that has values that are monotonic."
  [m]
  (and (sorted-map? m)
    (or (empty? m)
      (let [v (vals m)]
        (= v (sort v))))))

(s/fdef sorted-map-monotonic?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-strictly-monotonic?
  "Tests whether `m` is a 'sorted map strictly monotonic'. A 'sorted map
  strictly monotonic' is a sorted map (by keys) that has values that are
  strictly monotonic."
  [m]
  (and (sorted-map-monotonic? m)
    (or (empty? m)
      (apply distinct? (vals m)))))

(s/fdef sorted-map-strictly-monotonic?
  :args (s/cat :m any?)
  :ret boolean?)

(defn sorted-map-monotonic-by?
  "Tests whether `m` is a sorted map monotonic by."
  [comparator-k comparator-v m]
  (and (sorted-map-by? comparator-k m)
    (or (empty? m)
      (let [v (vals m)]
        (= v (sort comparator-v v))))))

(comment "documentation only"
  (s/fdef sorted-map-monotonic-by?
    :args (s/cat :comparator-k ::comparator
            :comparator-v ::comparator
            :m any?)
    :ret boolean?))

(defn sorted-map-strictly-monotonic-by?
  "Tests whether `m` is a sorted map strictly monotonic by."
  [comparator-k comparator-v m]
  (and (sorted-map-monotonic-by? comparator-k comparator-v m)
    (or (empty? m)
      (apply distinct? (vals m)))))

(comment "documentation only"
  (s/fdef sorted-map-strictly-monotonic-by?
    :args (s/cat :comparator-k ::comparator
            :comparator-v ::comparator
            :m any?)
    :ret boolean?))

(defmacro sorted-map-monotonic-of
  "This macro builds the spec for a sorted map monotonic."
  [kpred vpred min-count gen-max strictly?]
  (let [sform `(s/map-of ~kpred ~vpred
                 :min-count ~min-count)
        xform `(if ~strictly?
                 (s/and sorted-map-strictly-monotonic? ~sform)
                 (s/and sorted-map-monotonic? ~sform))]
    `(s/with-gen
       ~xform
       #(gen/let
          [i# (s/gen (s/int-in ~min-count (inc ~gen-max)))
           ks# (s/gen (s/coll-of ~kpred
                        :count i#
                        :distinct true))
           vs# (if ~strictly?
                 (s/gen (s/coll-of ~vpred
                          :count i#
                          :distinct true))
                 (s/gen (s/coll-of ~vpred
                          :count i#)))]
          (into (sorted-map) (zipmap (sort ks#) (sort vs#)))))))

(defmacro sorted-map-monotonic-by-of
  "This macro builds the spec for a sorted map monotonic by."
  [kpred vpred comparator-k comparator-v min-count gen-max strictly?]
  (let [sform `(s/map-of ~kpred ~vpred
                 :min-count ~min-count)
        xform `(if ~strictly?
                 (s/and
                   (partial sorted-map-strictly-monotonic-by?
                     ~comparator-k
                     ~comparator-v)
                   ~sform)
                 (s/and (partial sorted-map-monotonic-by?
                          ~comparator-k
                          ~comparator-v)
                   ~sform))]
    `(s/with-gen
       ~xform
       #(gen/let
          [i# (s/gen (s/int-in ~min-count (inc ~gen-max)))
           ks# (s/gen (s/coll-of ~kpred
                        :count i#
                        :distinct true))
           vs# (if ~strictly?
                 (s/gen (s/coll-of ~vpred
                          :count i#
                          :distinct true))
                 (s/gen (s/coll-of ~vpred
                          :count i#)))]
          (into (sorted-map-by ~comparator-k)
            (zipmap (sort-by ~comparator-k ks#) (sort-by ~comparator-v vs#)))))))

;;MAP MANIPULATION
(defn filter-map
  "Returns a filtered map containing only the key-value pairs that satisfy the predicate.
   
   Unlike clojure.core/filter (which returns a sequence), this maintains the map type
   and works with any map implementation (sorted maps, priority maps, etc.).
   
   Parameters:
   - pred-kv: A function taking key and value as arguments, returning boolean
   - m: The map to filter
   
   Example:
   ```clojure
   (filter-map (fn [k v] (> v 10)) {:a 5 :b 15 :c 20})
   ;; => {:b 15, :c 20}
   ```"
  [pred-kv m]
  (select-keys m
    (for [[k v] m :when (pred-kv k v)]
      k)))

(s/fdef filter-map
  :args (s/cat :pred-kv (s/fspec :args (s/cat :k any? :v any?)
                          :ret boolean?)
          :m map?)
  :ret map?)

(defn submap?
  "Checks whether map `m` contains all entries in map `sub`.
   
   Tests if `sub` is a subset of `m` by key-value pairs (not just keys).
   Uses Java's Map.containsAll for performance.
   
   Parameters:
   - m: The container map to check against
   - sub: The potential submap
   
   Returns true if every key-value pair in `sub` exists in `m`."
  [m sub]
  (.containsAll (.entrySet ^Map m) (.entrySet ^Map sub)))

(s/fdef submap?
  :args (s/cat :m map? :sub map?)
  :ret boolean?)

(defn select-keys-by
  "Returns a map containing only entries whose keys satisfy the predicate.
   Preserves the map type (sorted map, hash map, etc.).

   Parameters:
   - pred: A predicate function that takes a key
   - m: The map to filter

   Examples:
   ```clojure
   (select-keys-by keyword? {:a 1 \"b\" 2 :c 3})
   ;; => {:a 1 :c 3}

   (select-keys-by #(clojure.string/starts-with? (name %) \"user\")
                   {:user-id 1 :user-name \"bob\" :role \"admin\"})
   ;; => {:user-id 1 :user-name \"bob\"}
   ```"
  [pred m]
  (into (empty m)
    (filter (fn [[k _]] (pred k)))
    m))

(s/fdef select-keys-by
  :args (s/cat :pred (s/fspec :args (s/cat :k any?) :ret any?)
          :m map?)
  :ret map?)

(defn remove-keys-by
  "Returns a map with entries removed whose keys satisfy the predicate.
   Preserves the map type. Complement of [[select-keys-by]].

   Parameters:
   - pred: A predicate function that takes a key
   - m: The map to filter

   Examples:
   ```clojure
   (remove-keys-by keyword? {:a 1 \"b\" 2 :c 3})
   ;; => {\"b\" 2}

   (remove-keys-by #(clojure.string/starts-with? (name %) \"_\")
                   {:_id 1 :name \"bob\" :_meta {}})
   ;; => {:name \"bob\"}
   ```"
  [pred m]
  (select-keys-by (complement pred) m))

(s/fdef remove-keys-by
  :args (s/cat :pred (s/fspec :args (s/cat :k any?) :ret any?)
          :m map?)
  :ret map?)

(defn remove-map
  "Returns a filtered map excluding key-value pairs that satisfy the predicate.
   Complement of [[filter-map]]. Preserves the map type.

   Parameters:
   - pred-kv: A function taking key and value as arguments, returning boolean
   - m: The map to filter

   Example:
   ```clojure
   (remove-map (fn [k v] (> v 10)) {:a 5 :b 15 :c 20})
   ;; => {:a 5}
   ```"
  [pred-kv m]
  (filter-map (complement pred-kv) m))

(s/fdef remove-map
  :args (s/cat :pred-kv (s/fspec :args (s/cat :k any? :v any?)
                          :ret boolean?)
          :m map?)
  :ret map?)

(defn invert-map
  "Returns a map with keys and values swapped.

   Note: If the original map has duplicate values, only one will be preserved
   in the result (which one is undefined for hash maps).

   Parameters:
   - m: The map to invert

   Examples:
   ```clojure
   (invert-map {:a 1 :b 2 :c 3})
   ;; => {1 :a, 2 :b, 3 :c}

   (invert-map {1 :a 2 :b})
   ;; => {:a 1, :b 2}
   ```"
  [m]
  (into {} (map (fn [[k v]] [v k])) m))

(s/fdef invert-map
  :args (s/cat :m map?)
  :ret map?)

(defn map-kv
  "Transforms both keys and values of a map simultaneously.
   The function receives key and value, and must return a [new-key new-value] pair.

   Parameters:
   - f: A function taking (key, value) and returning [new-key new-value]
   - m: The map to transform

   Examples:
   ```clojure
   (map-kv (fn [k v] [(name k) (inc v)]) {:a 1 :b 2})
   ;; => {\"a\" 2, \"b\" 3}

   (map-kv (fn [k v] [v k]) {:a 1 :b 2})  ; invert
   ;; => {1 :a, 2 :b}
   ```"
  [f m]
  (into {} (map (fn [[k v]] (f k v))) m))

(comment "documentation only -- causes instrumentation difficulties for users"
  (s/fdef map-kv
    :args (s/cat :f (s/fspec :args (s/cat :k any? :v any?)
                      :ret (s/tuple any? any?))
            :m map?)
    :ret map?))

(defn update-some
  "Like `update` but only applies `f` if the key exists in the map.
   Avoids creating entries with nil values for missing keys.

   Parameters:
   - m: The map to update
   - k: The key to update
   - f: The function to apply to the existing value
   - args: Additional arguments to pass to f

   Examples:
   ```clojure
   (update-some {:a 1 :b 2} :a inc)
   ;; => {:a 2 :b 2}

   (update-some {:a 1 :b 2} :c inc)
   ;; => {:a 1 :b 2}  ; :c not added

   (update-some {:a 1} :a + 10 20)
   ;; => {:a 31}
   ```"
  [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(s/fdef update-some
  :args (s/cat :m map?
          :k any?
          :f ifn?
          :args (s/* any?))
  :ret map?)

