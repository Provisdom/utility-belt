(def project 'provisdom/utility-belt)
(def version "0.1.1")

(set-env!
  :source-paths #{"test"}
  :resource-paths #{"src"}
  :dependencies '[[provisdom/boot-tasks "1.2" :scope "tests"]
                  [provisdom/test "0.1.0" :scope "test"]
                  [midje "1.9.0-alpha5" :scope "test" :exclusions [org.clojure/clojure]]

                  [org.clojure/clojure "1.9.0-alpha13" :scope "provided"]
                  [org.clojure/core.async "0.2.391"]
                  [org.apache.commons/commons-lang3 "3.1"]])

(require
  '[provisdom.boot-tasks.core :refer :all])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom utility-belt"
       :url         "https://gitlab.com/provisdom/utility-belt"
       :scm         {:url "https://gitlab.com/provisdom/utility-belt"}
       :license     {"Provisdom" "(c) 2015 Provisdom Inc."}})