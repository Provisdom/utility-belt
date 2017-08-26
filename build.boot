(def project 'provisdom/utility-belt)
(def version "0.1.2-alpha2")

(set-env!
  :source-paths #{"test"}
  :resource-paths #{"src"}
  :dependencies '[[org.clojure/tools.nrepl "0.2.13" :scope "test"]
                  [org.clojure/test.check "0.9.0"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [midje "1.9.0-alpha8" :exclusions [org.clojure/clojure] :scope "test"]
                  [criterium "0.4.4" :scope "test"]

                  [provisdom/boot-tasks "1.4" :scope "tests"]
                  [provisdom/test "0.3.2" :scope "test"]
                  ;;project deps
                  [org.clojure/clojure "1.9.0-alpha17" :scope "provided"]
                  [org.clojure/spec.alpha "0.1.123"]
                  [org.clojure/core.async "0.3.443"]
                  [org.apache.commons/commons-lang3 "3.1"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer :all])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom utility-belt"
       :url         "https://gitlab.com/provisdom/utility-belt"
       :scm         {:url "https://gitlab.com/provisdom/utility-belt"}
       :license     {"Provisdom" "(c) 2015-2017 Provisdom Corporation"}})