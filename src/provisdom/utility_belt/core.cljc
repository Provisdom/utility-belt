(ns provisdom.utility-belt.core
  (:import (java.lang.reflect Method)))

(set! *warn-on-reflection* true)

;;;;(:refer-clojure :exclude [* - + / vector? ==])
;;;;(:use ;spyscope.core
;;;;clojure.java.javadoc


;;;;(set! *unchecked-math* true) ;only set this if you know for sure that 
;;;;    there will be no overflow
#_[can use this notation to comment within a line]

;;;;TODO:

;;;;move the double-layered stuff out into its own ns, and into math or 
;;;;    something (figure out how to use it clojure.core.matrix)
;;;;same thing with arrays; 2D-arrays should have own ns where they can 
;;;;    build on top of clojure.core.matrix

;;;;Matrix: transient fns? DoubleMatrix should have factory that inputs 
;;;;    FastVectors that are created fast
;;;;Matrix: create FAST-LONG-ARRAYS and LONG Matrices
;;;;Matrix: use core.matrix protocols too
;;;;Math: still need to figure out what to wrap for solvers...
;;;;defonce definterface "lisp, functional programming, and the state of flow"
;;;;dir, doc, source, pst, apropos, find-doc; T
;;;;    ry clicking in other eclipse windows;
;;;;#' instead of eval (causing n-dim error?); ' vs `
;;;;Group-by, into, frequencies etc use transient, which can't be parallel
;;;;Why can transient coll only be changed once?
;;;;Why commute f called twice?, isn't it how/what to force ref/atom signature? 
;;;;    Alt: just use ref?
;;;;clojure for patents, see old programming notes for coding ns

;;;;don't throw excecptions unless have to, use nil, nan, and zero for pdf e.g.
;;;;problem with using ^long in fn is that it rounds doubles to a long
;;;;when it could be confused with a double, don't use ^long then.
;;;;alternative is to ask m/roughly-round? and then use explicit cast
;;;;but use exceptions when needing to insist on long, boolean, etc.
;;;;write better descriptions, use wiki reference, etc;
;;;;R library, application, dog food, income
;;;;http://dev.clojure.org/display/doc/Clojure+Contrib+Libraries
;;;;look through Incanter
;;;;look in NewVersion emails (other math help)
;;;;do the example in the end of the first book
;;;;more solvers
;;;;;Robert Hooke library lets you extend functions
;;;;DEBUGGING: spyscope, Eclipse debugging, debugger library (see emails)
;;;;OPTIMIZATION: reducers, core.memoize, criterium for benchmarking, 
;;;;    'For' loops with pmap, p461 of second book: model configuration
;;;;TESTING: midje http://vimeo.com/19404746, return nil instead of error 
;;;;    when possible, Robert Hooke library could be useful, 
;;;;    also test.generative
;;;;TESTING: after testing, add core.typed for static typing

;;;;use fold-random on bayesian.regression too 
;;;;   (3 functions use lazy-rnd I think)
;;;;Must be something wrong with bracketed root solver; 
;;;;   Should be able to start with inv-cdf and send to solver, implement Harris
;;;;for stochastic integration, 
;;;;   use change-of-variable funcion from pro.math.calculus

;;;MACROS
(defmacro if-all-let
  ([bindings then else]
   (reduce (fn [subform binding]
             `(if-let [~@binding] ~subform ~else))
           then (reverse (partition 2 bindings))))
  ([bindings then]
   `(if-all-let ~bindings ~then nil)))

(defmacro when-all-let [bindings then]
  `(if-all-let ~bindings ~then nil))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn filter-map [pred m] (select-keys m (for [[k v] m :when (pred k v)] k)))

(defn sort-map [m] (apply sorted-map (flatten (into [] m))))

;;; TODO - is this necessary to disambiguate between case where key exists and has nil value vs. key does not exist?
(defn update-in-ext
  [m [k & ks] f not-found & args]
  (if ks
    (assoc m k (apply update-in-ext (get m k not-found) ks f not-found args))
    (assoc m k (apply f (get m k not-found) args))))

;;;SEQUENCE MANIPULATIONS
(defn concat-by-index
  "Returns a lazy sequence constructed by concatenating two collections with 
   c2 beginning at index 'i'.  
Preference goes to c2 and empty spaces are filled with 'nil'."
  [c1 c2 i]
  (lazy-seq
    (cond
      (and (empty? c1) (empty? c2)) c2
      (zero? i) (if (empty? c2)
                  (cons (first c1) (concat-by-index (rest c1) '() 0))
                  (cons (first c2) (concat-by-index (rest c1) (rest c2) i)))
      (neg? i) (cons (first c2) (concat-by-index c1 (rest c2) (inc i)))
      (pos? i) (cons (first c1) (concat-by-index (rest c1) c2 (dec i))))))

;;;REDUCE-KV EXTENSIONS AND REDUCTIONS-KV
(defn reduce-kv-ext
  "Extension of clojure's 'reduce-kv'.  
First collection must be the shortest.  
Function f takes the result value, an index, and the item value(s)"
  ([f init coll]
   (reduce-kv f init coll))
  ([f init c1 c2]
   (let [a1 (to-array c1) a2 (to-array c2)]
     (areduce a1 i ret init (f ret i (aget a1 i) (aget a2 i)))))
  ([f init c1 c2 c3]
   (let [a1 (to-array c1) a2 (to-array c2) a3 (to-array c3)]
     (areduce a1 i ret init (f ret i (aget a1 i) (aget a2 i) (aget a3 i))))))

;;;perhaps upgrade these by using 'reduced'
(defn reduce-kv-with-stop
  "Reduces a sequence using stopping predicates.  
Function f and predicates take the result value, an index, 
   and the item value(s)"
  ([f init coll stop-pred]
   (reduce-kv-with-stop f init coll stop-pred nil nil))
  ([f init c1 c2 stop-pred]
   (reduce-kv-with-stop f init c1 c2 stop-pred nil nil))
  ([f init coll stop-pred err-pred err-return-fn]
   (loop [i 0 [h & t] coll res init]
     (cond
       (or (not h) (stop-pred res i h)) res
       (and err-pred (err-pred res i h)) (err-return-fn res i h)
       :else (recur (inc i) t (f res i h)))))
  ([f init c1 c2 stop-pred err-pred err-return-fn]
   (loop [i 0 [h1 & t1] c1 [h2 & t2] c2 res init]
     (cond
       (or (not h1) (not h2) (stop-pred res i h1 h2)) res
       (and err-pred (err-pred res i h1 h2)) (err-return-fn res i h1 h2)
       :else (recur (inc i) t1 t2 (f res i h1 h2)))))
  ([f init c1 c2 c3 stop-pred err-pred err-return-fn]
   (loop [i 0 [h1 & t1] c1 [h2 & t2] c2 [h3 & t3] c3 res init]
     (cond
       (or (not h1) (not h2) (not h3) (stop-pred res i h1 h2 h3)) res
       (and err-pred (err-pred res i h1 h2 h3)) (err-return-fn res i h1 h2 h3)
       :else (recur (inc i) t1 t2 t3 (f res i h1 h2 h3))))))

(defn reductions-kv
  "Returns a lazy seq of a reduction with indices.  
Function f takes the result value, an index, and the item value(s)"
  ([f init coll]
   (when-all-let [[init-h & init-t] (seq coll)]
                 (letfn [(g [i res coll]
                           (cons res
                                 (lazy-seq
                                   (when-let [[h & t] (seq coll)]
                                     (g (inc i) (f res i h) t)))))]
                   (g 1 (f init 0 init-h) init-t))))
  ([f init c1 c2]
   (when-all-let [[init-h1 & init-t1] (seq c1) [init-h2 & init-t2] (seq c2)]
                 (letfn [(g [i res c1 c2]
                           (cons res
                                 (lazy-seq
                                   (when-all-let [[h1 & t1] (seq c1) [h2 & t2] (seq c2)]
                                                 (g (inc i) (f res i h1 h2) t1 t2)))))]
                   (g 1 (f init 0 init-h1 init-h2) init-t1 init-t2))))
  ([f init c1 c2 c3]
   (when-all-let [[init-h1 & init-t1] (seq c1) [init-h2 & init-t2] (seq c2)
                  [init-h3 & init-t3] (seq c3)]
                 (letfn [(g [i res c1 c2 c3]
                           (cons res
                                 (lazy-seq
                                   (when-all-let [[h1 & t1] (seq c1) [h2 & t2] (seq c2)
                                                  [h3 & t3] (seq c3)]
                                                 (g (inc i) (f res i h1 h2 h3) t1 t2 t3)))))]
                   (g 1 (f init 0 init-h1 init-h2 init-h3) init-t1 init-t2 init-t3)))))

;;;FLIP LAYERING
(defn flip-dbl-layered
  "Returns a double-layered that has layering flipped"
  [dbl-layered]
  (loop [flip '() idx (dec (count (first dbl-layered)))]
    (if (neg? idx)
      flip
      (recur (cons (map #(nth % idx) dbl-layered) flip) (dec idx)))))

;;;MAP EXTENSIONS
(defn map-dbl-layered [f dbl-layered] (map (partial apply f) dbl-layered))

(defn map-indexed-ext
  "Extension of clojure's 'map-indexed'.  
Function f takes the index and the item value(s).  
First collection must be the shortest."
  ([f coll]
   (map-indexed f coll))
  ([f c1 c2]
   (let [a1 (to-array c1) a2 (to-array c2)]
     (lazy-seq (amap a1 i ret (f i (aget a1 i) (aget a2 i))))))
  ([f c1 c2 c3]
   (let [a1 (to-array c1) a2 (to-array c2) a3 (to-array c3)]
     (lazy-seq (amap a1 i ret (f i (aget a1 i) (aget a2 i) (aget a3 i)))))))

(defn map-dbl-indexed
  "For double-layered sequences.  
Function f takes two indexes and the item value(s).  
First collection must be the shortest."
  ([f dbl-layered]
   (map-indexed
     (fn [outer-idx outer]
       (map-indexed
         (fn [inner-idx item]
           (f outer-idx inner-idx item)) outer)) dbl-layered))
  ([f d1 d2]
   (map-indexed-ext
     (fn [outer-idx o1 o2]
       (map-indexed-ext
         (fn [inner-idx i1 i2] (f outer-idx inner-idx i1 i2)) o1 o2)) d1 d2))
  ([f d1 d2 d3]
   (map-indexed-ext
     (fn [outer-idx o1 o2 o3]
       (map-indexed-ext
         (fn [inner-idx i1 i2 i3]
           (f outer-idx inner-idx i1 i2 i3)) o1 o2 o3)) d1 d2 d3)))

(defn map-triple-indexed
  "For triple-layered sequences.  
Function f takes three indexes and the item value(s).  
First collection must be the shortest."
  ([f triple-layered]
   (map-indexed
     (fn [outer-idx outer]
       (map-indexed
         (fn [middle-idx middle]
           (map-indexed
             (fn [inner-idx item] (f outer-idx middle-idx inner-idx item))
             middle))
         outer))
     triple-layered))
  ([f t1 t2]
   (map-indexed-ext (fn [outer-idx o1 o2]
                      (map-indexed-ext
                        (fn [middle-idx m1 m2]
                          (map-indexed-ext
                            (fn [inner-idx i1 i2]
                              (f outer-idx middle-idx inner-idx i1 i2))
                            m1 m2))
                        o1 o2))
                    t1 t2))
  ([f t1 t2 t3]
   (map-indexed-ext
     (fn [outer-idx o1 o2 o3]
       (map-indexed-ext
         (fn [middle-idx m1 m2 m3]
           (map-indexed-ext (fn [inner-idx i1 i2 i3]
                              (f outer-idx middle-idx inner-idx i1 i2 i3))
                            m1 m2 m3))
         o1 o2 o3))
     t1 t2 t3)))

;;;MAPCAT-INDEXED
(defn mapcat-indexed
  "Returns the result of applying concat to the result of applying map-indexed to f and colls.
Thus function f should return a collection."
  [f & colls]
  (apply concat (apply map-indexed f colls)))

;;;VECTOR MANIPULATION
(defn insertv
  "Returns a vector with the new value inserted into index"
  [coll value index]
  (let [f (subvec coll 0 index) l (subvec coll index)]
    (vec (concat f [value] l))))

(defn removev
  "Returns a vector with the value in the index removed"
  [coll index]
  (let [f (subvec coll 0 index) l (subvec coll (inc index))]
    (vec (concat f l))))

;;;SEQ CREATION EXTENSIONS
;;these can be nearly replaced with mx/compute 
;;(can move to mx and use with :sequence implementation)
(defn create-seq
  [size f] (map-indexed (fn [i _] (f i)) (repeat size 0)))

(defn create-dbl-layered
  [outer inner f]
  (map-dbl-indexed (fn [i j _] (f i j))
                   (partition inner (repeat (* outer inner) 0))))

(defn create-triple-layered
  [outer middle inner f]
  (map-triple-indexed
    (fn [i j k _] (f i j k))
    (partition middle (partition inner (repeat (* outer middle inner) 0)))))

;;;DEEP nth
(defn deep-nth
  ([dbl-layered i j] (-> dbl-layered (nth i) (nth j)))
  ([triple-layered i j k] (-> triple-layered (nth i) (nth j) (nth k))))

;;;PARTITIONS
(defn partition-recursively
  "Partitions recursively in sets of 'n'.  
For example, a 100-element could be double-layered into 10s"
  [coll n]
  (last (take (Math/round (/ (Math/log (count coll)) (Math/log n)))
              (iterate #(partition n %) coll))))

;;;FOR EXT
(defn for-ext*                                              ;replace with map f cartesian dbl-layered (or use mx/compute)
  "Returns a double-layered that has been expanded.  
Each original row represents a series of values to be part of a list 
   comprehension.
So, e.g., three rows of 10 elements each would return one-thousand elements.
Function f takes a sequence with an element for each original row.
This is useful for when there is potentially many rows, since that would 
   require a 'for' loop for each."
  [f dbl-layered]
  (let [nrow (count dbl-layered) ncol (count (first dbl-layered))]
    (for [idx (range (Math/pow ncol nrow))]
      (f (for [v (range nrow)]
           (let [p (- nrow v)
                 j (quot (rem idx (Math/pow ncol p)) (Math/pow ncol (dec p)))]
             (deep-nth dbl-layered v j)))))))

;;;FUNCTIONALS
(defn in-place-functional
  ([f-coll arg-coll] (map (fn [f a] (f a)) f-coll arg-coll))
  ([f-coll ac1 ac2] (map (fn [f a b] (f a b)) f-coll ac1 ac2))
  ([f-coll ac1 ac2 ac3] (map (fn [f a b c] (f a b c)) f-coll ac1 ac2 ac3)))

;;;ARITIES
(defn arities
  "Uses reflection to return the arity number of the function 'f'"
  [f]
  (let [all-declared-methods (.getDeclaredMethods (class f))
        methods-named (fn [name] (filter #(= (.getName ^Method %) name)
                                         all-declared-methods))
        methods-named-invoke (methods-named "invoke")
        methods-named-do-invoke (methods-named "doInvoke")
        is-rest-fn (seq methods-named-do-invoke)
        x (sort (map #(alength (.getParameterTypes ^Method %)) methods-named-invoke))]
    (if is-rest-fn (conj (vec x) :rest) x)))

;;;CHANGING DEFAULT FUNCTION BEHAVIOR WITH NILS
(defn ignore-nils
  "Applies function 'f' and ignores any nil values in 'x'."
  [f & x]
  (apply f (remove nil? x)))

(defn throw-nils
  "Applies function 'f' and throws error on any nil value in 'x' or on nil return value."
  [f & x]
  (let [ex (throw (ex-info "nil not allowed" {:fn (var throw-nils)}))]
    (if (some nil? x) ex (or (apply f x) ex))))

(defn nil-nils
  "Applies function 'f' and returns nil on any nil value."
  [f & x]
  (if (some nil? x) nil (apply f x)))

;;;MAYBE FIRST
(defn maybe-first [x] (if (sequential? x) (first x) x))     ; TODO - think about design which requires this function. Is there a better way?

(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second etc.
Difference from interleave is that all elements are consumed."
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (cond (and s1 s2) (cons (first s1)
                               (cons (first s2) (interleave-all
                                                  (rest s1) (rest s2))))
             s1 s1
             s2 s2))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (map seq (conj colls c2 c1))]
       (if (every? identity ss)
         (concat (map first ss) (apply interleave-all (map rest ss)))
         (let [ns (filter identity ss)]
           (concat (map first ns) (apply interleave-all (map rest ns)))))))))

;;;ADDED BACK
(defn replace-nils
  "Returns a lazy sequence constructed by replacing nil values"
  [[h & t :as coll] r-coll]
  (if (or (empty? r-coll) (empty? coll))
    coll
    (lazy-seq
      (if (nil? h)
        (cons (first r-coll) (replace-nils t (rest r-coll)))
        (cons h (replace-nils t r-coll))))))

;;ADDED NEW
(defn submap?
  "Checks whether m contains all entries in sub."
  [^java.util.Map m ^java.util.Map sub]
  (.containsAll (.entrySet m) (.entrySet sub)))

(defn fmap
  "Maps a function onto the values of a map."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn index-of [coll item]
  (count (take-while (partial not= item) coll)))