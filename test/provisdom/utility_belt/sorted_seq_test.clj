(ns provisdom.utility-belt.sorted-seq-test
  (:require
    [clojure.test :as ct]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.sorted-seq :as sorted-seq]))

;1 seconds

(set! *warn-on-reflection* true)

(ct/deftest list-sorted?-test
  (t/is-not (sorted-seq/list-sorted? [:a :b]))
  (is (sorted-seq/list-sorted? '(:a :b)))
  (t/is-not (sorted-seq/list-sorted? '(:b :a))))

(ct/deftest list-sorted-by?-test
  (t/is-not (sorted-seq/list-sorted-by? > [1 2]))
  (t/is-not (sorted-seq/list-sorted-by? > '(1 2)))
  (is (sorted-seq/list-sorted-by? < '(1 2))))

(ct/deftest vector-sorted?-test
  (is (sorted-seq/vector-sorted? [:a :b]))
  (t/is-not (sorted-seq/vector-sorted? '(:a :b)))
  (t/is-not (sorted-seq/vector-sorted? [:b :a])))

(ct/deftest vector-sorted-by?-test
  (t/is-not (sorted-seq/vector-sorted-by? > [1 2]))
  (t/is-not (sorted-seq/vector-sorted-by? > '(1 2)))
  (is (sorted-seq/vector-sorted-by? < [1 2])))
