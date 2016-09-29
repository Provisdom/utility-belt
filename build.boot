(def project 'provisdom/utility-belt)
(def version "0.1.1")

(set-env!
  :source-paths #{"src" "test"}
  :resource-paths #{"src"}
  :asset-paths #{}
  :dependencies '[[org.clojure/clojure "1.9.0-alpha13"]
                  [provisdom/boot-tasks "0.7.0" :scope "tests"]
                  [org.clojure/core.async "0.2.391"]
                  [org.apache.commons/commons-lang3 "3.1"]

                  [provisdom/test "0.1.0" :scope "test"]
                  [midje "1.9.0-alpha5" :scope "test" :exclusions [org.clojure/clojure]]])

(require
  '[provisdom.boot-tasks.core :refer :all])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom utility-belt"
       :url         "https://gitlab.com/provisdom/utility-belt"
       :scm         {:url "https://gitlab.com/provisdom/utility-belt"}
       :license     {"Provisdom" "(c) 2015 Provisdom Inc."}})