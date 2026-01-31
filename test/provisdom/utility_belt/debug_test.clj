(ns provisdom.utility-belt.debug-test
  (:require
    [clojure.string :as str]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.utility-belt.debug :as debug]))

;;1 second

(set! *warn-on-reflection* true)

;; *debug-enabled*
(t/deftest debug-enabled-test
  (t/with-instrument :all
    ;; enabled by default
    (t/is debug/*debug-enabled*)
    ;; can be bound to false
    (binding [debug/*debug-enabled* false]
      (t/is-not debug/*debug-enabled*))))

;; dbg
(t/deftest dbg-test
  (t/with-instrument `debug/dbg
    (t/is-spec-check debug/dbg))
  (t/with-instrument :all
    ;; returns the value
    (t/is= {::anomalies/message "Test"} (debug/dbg {::anomalies/message "Test"}))
    (t/is= 42 (debug/dbg 42))
    ;; prints output
    (let [output (with-out-str (debug/dbg (+ 1 2)))]
      (t/is (str/includes? output "dbg:"))
      (t/is (str/includes? output "3")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/dbg (+ 1 2))))]
      (t/is= "" output))))

;; dbg-pp
(t/deftest dbg-pp-test
  (t/with-instrument `debug/dbg-pp
    (t/is-spec-check debug/dbg-pp))
  (t/with-instrument :all
    ;; returns the value
    (t/is= {:nested {:data [1 2 3]}} (debug/dbg-pp {:nested {:data [1 2 3]}}))
    ;; prints output
    (let [output (with-out-str (debug/dbg-pp {:a 1}))]
      (t/is (str/includes? output "dbg:"))
      (t/is (str/includes? output ":a")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/dbg-pp {:a 1})))]
      (t/is= "" output))))

;; dbg-when
(t/deftest dbg-when-test
  (t/with-instrument `debug/dbg-when
    (t/is-spec-check debug/dbg-when))
  (t/with-instrument :all
    ;; always returns the value
    (t/is= 5 (debug/dbg-when pos? 5))
    (t/is= -5 (debug/dbg-when pos? -5))
    (t/is= nil (debug/dbg-when some? nil))
    ;; only prints when predicate is true
    (let [output (with-out-str (debug/dbg-when pos? 5))]
      (t/is (str/includes? output "dbg-when:")))
    (let [output (with-out-str (debug/dbg-when pos? -5))]
      (t/is= "" output))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/dbg-when pos? 5)))]
      (t/is= "" output))))

;; spy
(t/deftest spy-test
  (t/with-instrument `debug/spy
    (t/is-spec-check debug/spy))
  (t/with-instrument :all
    ;; returns the value
    (t/is= 42 (debug/spy :test 42))
    ;; prints labeled output
    (let [output (with-out-str (debug/spy :my-label {:a 1}))]
      (t/is (str/includes? output ":my-label"))
      (t/is (str/includes? output "=>")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/spy :test 42)))]
      (t/is= "" output))))

;; timed
(t/deftest timed-test
  (t/with-instrument `debug/timed
    (t/is-spec-check debug/timed))
  (t/with-instrument :all
    ;; returns the value
    (t/is= 6 (debug/timed :add (+ 1 2 3)))
    ;; prints timing output
    (let [output (with-out-str (debug/timed :calc (reduce + (range 100))))]
      (t/is (str/includes? output ":calc"))
      (t/is (str/includes? output "took"))
      (t/is (str/includes? output "ms")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/timed :calc (+ 1 2))))]
      (t/is= "" output))))

;; tap-dbg
(t/deftest tap-dbg-test
  (t/with-instrument `debug/tap-dbg
    (t/is-spec-check debug/tap-dbg))
  (t/with-instrument :all
    ;; returns the value
    (t/is= 42 (debug/tap-dbg 42))
    ;; sends to tap>
    (let [tapped (atom nil)
          tap-fn (fn [v] (reset! tapped v))]
      (add-tap tap-fn)
      (try
        (debug/tap-dbg {:test "value"})
        (Thread/sleep 10)
        (t/is= {:test "value"} @tapped)
        (finally
          (remove-tap tap-fn))))
    ;; not affected by *debug-enabled*
    (let [tapped (atom nil)
          tap-fn (fn [v] (reset! tapped v))]
      (add-tap tap-fn)
      (try
        (binding [debug/*debug-enabled* false]
          (debug/tap-dbg {:still "tapped"}))
        (Thread/sleep 10)
        (t/is= {:still "tapped"} @tapped)
        (finally
          (remove-tap tap-fn))))))

;; tap-spy
(t/deftest tap-spy-test
  (t/with-instrument `debug/tap-spy
    (t/is-spec-check debug/tap-spy))
  (t/with-instrument :all
    ;; returns the value
    (t/is= 42 (debug/tap-spy :test 42))
    ;; sends labeled value to tap>
    (let [tapped (atom nil)
          tap-fn (fn [v] (reset! tapped v))]
      (add-tap tap-fn)
      (try
        (debug/tap-spy :my-label {:data 123})
        (Thread/sleep 10)
        (t/is= {:label :my-label :value {:data 123}} @tapped)
        (finally
          (remove-tap tap-fn))))))

;; tap-timed
(t/deftest tap-timed-test
  (t/with-instrument `debug/tap-timed
    (t/is-spec-check debug/tap-timed))
  (t/with-instrument :all
    ;; returns the value
    (t/is= 6 (debug/tap-timed :add (+ 1 2 3)))
    ;; sends timing info to tap>
    (let [tapped (atom nil)
          tap-fn (fn [v] (reset! tapped v))]
      (add-tap tap-fn)
      (try
        (debug/tap-timed :calc (reduce + (range 100)))
        (Thread/sleep 10)
        (t/is= :calc (:label @tapped))
        (t/is (number? (:elapsed-ms @tapped)))
        (t/is= 4950 (:value @tapped))
        (finally
          (remove-tap tap-fn))))))

;; dbg-fn
(t/deftest dbg-fn-test
  (t/with-instrument `debug/dbg-fn
    (t/is-spec-check debug/dbg-fn))
  (t/with-instrument :all
    ;; wrapped function returns correct value
    (let [traced-add (debug/dbg-fn :add +)]
      (t/is= 6 (traced-add 1 2 3)))
    ;; prints call and return
    (let [traced-inc (debug/dbg-fn :inc inc)
          output (with-out-str (traced-inc 5))]
      (t/is (str/includes? output ":inc"))
      (t/is (str/includes? output "called with:"))
      (t/is (str/includes? output "returned:")))
    ;; respects *debug-enabled*
    (let [traced-inc (debug/dbg-fn :inc inc)
          output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (traced-inc 5)))]
      (t/is= "" output))))

;; spy->
(t/deftest spy->-test
  (t/with-instrument `debug/spy->
    (t/is-spec-check debug/spy->))
  (t/with-instrument :all
    ;; returns correct value through pipeline
    (t/is= 9 (debug/spy-> 1 (+ 2) (* 3)))
    ;; single value
    (t/is= 42 (debug/spy-> 42))
    ;; prints each step
    (let [output (with-out-str (debug/spy-> 1 (+ 2) (* 3)))]
      (t/is (str/includes? output "spy->"))
      (t/is (str/includes? output "1"))
      (t/is (str/includes? output "3"))
      (t/is (str/includes? output "9")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/spy-> 1 (+ 2) (* 3))))]
      (t/is= "" output))))

;; spy->>
(t/deftest spy->>-test
  (t/with-instrument `debug/spy->>
    (t/is-spec-check debug/spy->>))
  (t/with-instrument :all
    ;; returns correct value through pipeline
    (t/is= [2 4] (debug/spy->> (range 5) (map inc) (filter even?) vec))
    ;; single value
    (t/is= [0 1 2] (debug/spy->> (range 3) vec))
    ;; prints each step
    (let [output (with-out-str (debug/spy->> (range 3) (map inc) (filter odd?)))]
      (t/is (str/includes? output "spy->>")))
    ;; respects *debug-enabled*
    (let [output (with-out-str
                   (binding [debug/*debug-enabled* false]
                     (debug/spy->> (range 3) (map inc))))]
      (t/is= "" output))))

