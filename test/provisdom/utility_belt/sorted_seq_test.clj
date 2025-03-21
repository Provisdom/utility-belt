(ns provisdom.utility-belt.sorted-seq-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.utility-belt.sorted-seq :as sorted-seq]))

;1 seconds

(set! *warn-on-reflection* true)

(deftest list-sorted?-test
  (is-not (sorted-seq/list-sorted? [:a :b]))
  (is (sorted-seq/list-sorted? '(:a :b)))
  (is-not (sorted-seq/list-sorted? '(:b :a))))

(deftest list-sorted-by?-test
  (is-not (sorted-seq/list-sorted-by? > [1 2]))
  (is-not (sorted-seq/list-sorted-by? > '(1 2)))
  (is (sorted-seq/list-sorted-by? < '(1 2))))

(deftest vector-sorted?-test
  (is (sorted-seq/vector-sorted? [:a :b]))
  (is-not (sorted-seq/vector-sorted? '(:a :b)))
  (is-not (sorted-seq/vector-sorted? [:b :a])))

(deftest vector-sorted-by?-test
  (is-not (sorted-seq/vector-sorted-by? > [1 2]))
  (is-not (sorted-seq/vector-sorted-by? > '(1 2)))
  (is (sorted-seq/vector-sorted-by? < [1 2])))
