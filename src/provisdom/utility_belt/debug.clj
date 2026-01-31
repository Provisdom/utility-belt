(ns provisdom.utility-belt.debug
  "Simple debugging utilities for printing intermediate values during development.

   All print-based debugging macros respect the `*debug-enabled*` dynamic var, allowing you to
   globally disable debug output with `(binding [*debug-enabled* false] ...)`.

   Tap-based macros ([[tap-dbg]], [[tap-spy]], [[tap-timed]]) always send to `tap>` regardless of
   `*debug-enabled*` since `tap>` listeners can filter as needed.

   Quick reference:
   - [[dbg]]/[[dbg-pp]] - inline inspection, prints code form and value
   - [[spy]] - pipeline debugging with custom label (use when code form would be a gensym)
   - [[spy->]]/[[spy->>]] - trace every step in a threading macro
   - [[timed]] - measure execution time with label (unlike `clojure.core/time`, can be disabled)
   - [[dbg-fn]] - wrap a function to trace its calls and returns
   - [[dbg-when]] - conditional debug output based on predicate
   - `tap-*` variants - same behavior but send to `tap>` for tools like Portal, REBL, Reveal"
  (:require
    [clojure.pprint :as pprint]))

(def ^:dynamic *debug-enabled*
  "Dynamic var to globally enable/disable print-based debug output. Bind to `false` to suppress
   all [[dbg]], [[spy]], [[timed]], etc. output. Tap-based macros are not affected by this setting."
  true)

(defmacro dbg
  "Wrap an expression with `(dbg expression)` to see it print out during debugging. Respects
   `*debug-enabled*`."
  [x]
  `(let [x# ~x]
     (when *debug-enabled*
       (println "dbg:" '~x "=" x#))
     x#))

(defmacro dbg-pp
  "Like [[dbg]] but uses pprint for complex nested data structures. Respects `*debug-enabled*`.

   Examples:
   ```clojure
   (dbg-pp {:nested {:data [1 2 3]}})
   ;; prints: dbg: {:nested {:data [1 2 3]}} =
   ;;         {:nested {:data [1 2 3]}}
   ;; (pretty-printed)
   ```"
  [x]
  `(let [x# ~x]
     (when *debug-enabled*
       (println "dbg:" '~x "=")
       (pprint/pprint x#))
     x#))

(defmacro dbg-when
  "Only print debug output when predicate returns `true` on the value. Always returns the value
   unchanged. Respects `*debug-enabled*`.

   Parameters:
   - `pred`: Predicate function to test the value
   - `x`: The expression to evaluate

   Examples:
   ```clojure
   (dbg-when some? (get-maybe-nil-value))
   ;; only prints if value is non-nil

   (dbg-when #(> % 100) (calculate-something))
   ;; only prints if result > 100
   ```"
  [pred x]
  `(let [x# ~x]
     (when (and *debug-enabled* (~pred x#))
       (println "dbg-when:" '~x "=" x#))
     x#))

(defmacro spy
  "Print a labeled value and return it. Useful for debugging pipelines. Respects `*debug-enabled*`.

   Parameters:
   - `label`: A label to identify the value (typically a keyword)
   - `x`: The expression to evaluate and print

   Examples:
   ```clojure
   (-> data
       (transform)
       (spy :after-transform)
       (process))
   ;; prints: :after-transform => <value>
   ;; returns the value unchanged
   ```"
  [label x]
  `(let [x# ~x]
     (when *debug-enabled*
       (println ~label "=>" (pr-str x#)))
     x#))

(defmacro timed
  "Measure and print execution time of an expression. Returns the value. Respects `*debug-enabled*`.

   Parameters:
   - `label`: A label to identify the timed operation
   - `x`: The expression to time

   Examples:
   ```clojure
   (timed :expensive-calc (reduce + (range 1000000)))
   ;; prints: :expensive-calc took 42.123 ms
   ;; returns the sum
   ```"
  [label x]
  `(let [start# (System/nanoTime)
         result# ~x
         elapsed# (/ (- (System/nanoTime) start#) 1e6)]
     (when *debug-enabled*
       (println ~label "took" elapsed# "ms"))
     result#))

(defmacro tap-dbg
  "Send value to `tap>` and return it unchanged. Integrates with tap-based debugging tools like
   Portal, REBL, or Reveal. Not affected by `*debug-enabled*`.

   Parameters:
   - `x`: The expression to evaluate and tap

   Examples:
   ```clojure
   (-> data
       (transform)
       (tap-dbg)  ; sends to tap> listeners
       (process))
   ```"
  [x]
  `(let [x# ~x]
     (tap> x#)
     x#))

(defmacro tap-spy
  "Send a labeled value to `tap>` and return the value unchanged. Combines [[tap-dbg]] with a label
  for easier identification. Not affected by `*debug-enabled*`.

   Parameters:
   - `label`: A label to identify the value
   - `x`: The expression to evaluate and tap

   Examples:
   ```clojure
   (-> data
       (transform)
       (tap-spy :after-transform)
       (process))
   ;; sends {:label :after-transform :value <value>} to tap> listeners
   ```"
  [label x]
  `(let [x# ~x]
     (tap> {:label ~label :value x#})
     x#))

(defmacro tap-timed
  "Measure execution time and send timing info to `tap>`. Returns the value. Not affected by
   `*debug-enabled*`.

   Parameters:
   - `label`: A label to identify the timed operation
   - `x`: The expression to time

   Examples:
   ```clojure
   (tap-timed :db-query (fetch-data db))
   ;; sends {:label :db-query :elapsed-ms 123.45 :value <result>} to tap>
   ```"
  [label x]
  `(let [start# (System/nanoTime)
         result# ~x
         elapsed# (/ (- (System/nanoTime) start#) 1e6)]
     (tap> {:elapsed-ms elapsed#, :label ~label, :value result#})
     result#))

(defn dbg-fn
  "Wrap a function to print its arguments and return value on each call. Useful for tracing function
  calls. Respects `*debug-enabled*`.

   Parameters:
   - `label`: A label to identify the function
   - `f`: The function to wrap

   Examples:
   ```clojure
   (def traced-add (dbg-fn :add +))
   (traced-add 1 2 3)
   ;; prints: :add called with: (1 2 3)
   ;; prints: :add returned: 6
   ;; returns: 6

   ;; Or wrap inline:
   (map (dbg-fn :process process-item) items)
   ```"
  [label f]
  (fn [& args]
    (when *debug-enabled*
      (println label "called with:" args))
    (let [result (apply f args)]
      (when *debug-enabled*
        (println label "returned:" result))
      result)))

(defmacro spy->
  "Debug version of `->` that prints each intermediate value. Respects `*debug-enabled*`.

   Examples:
   ```clojure
   (spy-> 1
          (+ 2)
          (* 3))
   ;; prints: spy-> 1 => 1
   ;; prints: spy-> (+ 2) => 3
   ;; prints: spy-> (* 3) => 9
   ;; returns: 9
   ```"
  [x & forms]
  (let [thread-form (fn [val form]
                      (if (seq? form)
                        `(~(first form) ~val ~@(rest form))
                        `(~form ~val)))
        gensyms (repeatedly (inc (count forms)) #(gensym "v"))
        init-sym (first gensyms)]
    (if (empty? forms)
      `(let [~init-sym ~x]
         (when *debug-enabled*
           (println "spy->" '~x "=>" ~init-sym))
         ~init-sym)
      (let [step-syms (rest gensyms)
            init-binding `[~init-sym ~x]
            step-bindings (mapcat (fn [prev-sym curr-sym form]
                                    `[~curr-sym ~(thread-form prev-sym form)
                                      ~'_ (when *debug-enabled*
                                            (println "spy->" '~form "=>" ~curr-sym))])
                            (cons init-sym (butlast step-syms))
                            step-syms
                            forms)]
        `(let [~@init-binding
               ~'_ (when *debug-enabled*
                     (println "spy->" '~x "=>" ~init-sym))
               ~@step-bindings]
           ~(last step-syms))))))

(defmacro spy->>
  "Debug version of `->>` that prints each intermediate value. Respects `*debug-enabled*`.

   Examples:
   ```clojure
   (spy->> (range 5)
           (map inc)
           (filter even?))
   ;; prints: spy->> (range 5) => (0 1 2 3 4)
   ;; prints: spy->> (map inc) => (1 2 3 4 5)
   ;; prints: spy->> (filter even?) => (2 4)
   ;; returns: (2 4)
   ```"
  [x & forms]
  (let [thread-form (fn [val form]
                      (if (seq? form)
                        `(~(first form) ~@(rest form) ~val)
                        `(~form ~val)))
        gensyms (repeatedly (inc (count forms)) #(gensym "v"))
        init-sym (first gensyms)]
    (if (empty? forms)
      `(let [~init-sym ~x]
         (when *debug-enabled*
           (println "spy->>" '~x "=>" ~init-sym))
         ~init-sym)
      (let [step-syms (rest gensyms)
            init-binding `[~init-sym ~x]
            step-bindings (mapcat (fn [prev-sym curr-sym form]
                                    `[~curr-sym ~(thread-form prev-sym form)
                                      ~'_ (when *debug-enabled*
                                            (println "spy->>" '~form "=>" ~curr-sym))])
                            (cons init-sym (butlast step-syms))
                            step-syms
                            forms)]
        `(let [~@init-binding
               ~'_ (when *debug-enabled*
                     (println "spy->>" '~x "=>" ~init-sym))
               ~@step-bindings]
           ~(last step-syms))))))
