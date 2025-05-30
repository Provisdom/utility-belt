(ns provisdom.utility-belt.nils
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.utility-belt.anomalies :as anomalies]))

(defn ignore-nils
  "Applies function `f` and ignores any nil values in `args`."
  [f & args]
  (apply f (remove nil? args)))

(s/fdef ignore-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn anomaly-nils
  "Applies function `f` and returns an anomaly on any nil value in `args` or on
  a nil return value."
  [f & args]
  (let [anomaly {::anomalies/category ::anomalies/forbidden
                 ::anomalies/message  "nil not allowed"
                 ::anomalies/fn       (var anomaly-nils)}]
    (if (some nil? args)
      anomaly
      (or (apply f args) anomaly))))

(s/fdef anomaly-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn nil-nils
  "Applies function `f` and returns nil on any nil value."
  [f & args]
  (when-not (some nil? args)
    (apply f args)))

(s/fdef nil-nils
  :args (s/cat :f (s/fspec :args (s/cat :x (s/* any?))
                    :ret any?)
          :args (s/* any?))
  :ret any?)

(defn replace-nils
  "Returns a lazy sequence constructed by replacing nil values."
  [[h & t :as coll] replacement-coll]
  (if (or (empty? replacement-coll) (empty? coll))
    coll
    (lazy-seq
      (if (nil? h)
        (cons (first replacement-coll) (replace-nils t (rest replacement-coll)))
        (cons h (replace-nils t replacement-coll))))))

(s/fdef replace-nils
  :args (s/cat :coll (s/every any?)
          :replacement-coll (s/every any?))
  :ret (s/every any?))
