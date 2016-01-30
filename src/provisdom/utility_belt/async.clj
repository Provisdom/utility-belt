(ns provisdom.utility-belt.async
  (:require [provisdom.utility-belt.core :as co]
            [clojure.core.async :as async]))

(set! *warn-on-reflection* true)

;DECLARATIONS
(declare )

(defn catch-error-or-nil
  "When error is caught, returns false by default.
Default-fn should take an Exception or Error and can return anything."
  ([f] (catch-error-or-nil f (fn [e] false)))
  ([f default-fn]
   (try (let [r (f)] (if (nil? r)
                       (throw (ex-info "nil result" {:fn (var catch-error-or-nil)}))
                       r))
        (catch Exception e (if (fn? default-fn) (default-fn e) default-fn))
        (catch Error e (if (fn? default-fn) (default-fn e) default-fn)))))

;;; TODO - has async monad written all over it
(defn thread 
  "Call each of the fs on a separate thread. 
By default, each of the fs are wrapped in a function that could throw an error 
   or return nil, or it will hang.  
By default, all functions are wrapped with the catch-error-or-nil function.
options for k:
   :and -- Returns false if any are falsey, otherwise returns vector with all 
           the results in order corresponding to their fs. 
        -- Short-circuits (and cancels the calls to remaining fs) on first 
           falsey value returned.
   :first!! -- Returns nil if all are falsey, otherwise returns value of first 
               truthy result.
            -- Short-circuits (and cancels the calls to remaining fs) on first 
               truthy value returned.
            -- This induces race conditions between threads and can lead to 
               inconsistent results. 
   :any-ordered -- Returns a tuple containing the [value index] as soon as any 
                   f returns truthy with the previous fs all having returned 
                   falsey.
   :all -- Returns a vector with all the results in order corresponding to 
           their fs.
   :or -- Returns true if any are truthy, otherwise returns false.
       -- Short-circuits (and cancels the calls to remaining fs) on first 
          truthy value returned."
  ([k fs] (thread catch-error-or-nil k fs))
  ([wrap-fn k fs]
    (let [futs (doall (for [f (map (fn [i g] [i #(wrap-fn g)]) (range) fs)]
                        (let [c (async/chan)] 
                          [(future (async/>!! c ((second f)))) c (first f)])))]
      (loop [futs futs, res (vec fs)]
        (if (seq futs)
          (let [[result c] (async/alts!! (map second futs) :priority true),
                i (peek (first (filter #(= c (second %)) futs))),
                res (assoc res i result), 
                f-on #(remove (fn [e] (identical? (peek e) i)) futs),
                f-off #(doseq [fut (map first futs)] (future-cancel fut))]
            (condp = k
              :and (if result (recur (f-on) res) (do (f-off) false))
              :first!! (if-not result (recur (f-on) res) (do (f-off) result))
              :or (if-not result (recur (f-on) res) (do (f-off) true))
              :any-ordered (if-not (and result 
                                        (not-any? fn? (take (inc i) res))) 
                             (recur (f-on) res) (do (f-off) [(nth res i) i]))
              :all (recur (f-on) res)
              :else (throw (ex-info (str "Invalid option: " k) {:fn (var thread)}))))
          (if (or (= k :all) (= k :and)) res false))))))

(defn thread-select 
  ([selector-fn fs] (thread-select selector-fn catch-error-or-nil fs))
  ([selector-fn wrap-fn fs] (let [res (filter #(not (not %)) 
                                              (thread wrap-fn :all fs))] 
                              (if (first res) (selector-fn res) nil))))

(defn thread-max [fs] (thread-select #(apply max %) fs))

(defn thread-min [fs] (thread-select #(apply min %) fs))         