(ns provisdom.utility-belt.anomalies
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;;ANOMALIES
;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS-IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(s/def ::category #{::unavailable
                    ::interrupted
                    ::incorrect
                    ::forbidden
                    ::unsupported
                    ::not-found
                    ::conflict
                    ::fault
                    ::busy
                    ::third-party
                    ::no-solve
                    ::exception
                    ::error})

(s/def ::message string?)

(s/def ::anomaly (s/keys :req [::category]
                         :opt [::message
                               ::fn]))

(defn anomaly?
  "Tests whether `x` is an anomaly."
  [x]
  (s/valid? ::anomaly x))

(s/fdef anomaly?
        :args (s/cat :x any?)
        :ret boolean?)