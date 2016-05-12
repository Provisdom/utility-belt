(def project 'provisdom/utility-belt)
(def version "0.1.0")

(set-env!
  :source-paths #{"src" "test"}
  :resource-paths #{"src"}
  :asset-paths #{}
  :dependencies '[[org.clojure/clojure "1.8.0"]
                  [provisdom/boot-tasks "0.6.0" :scope "tests"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                  [org.apache.commons/commons-lang3 "3.1"]

                  [provisdom/test "0.1.0" :scope "test"]
                  [midje "1.8.3" :scope "test" :exclusions [org.clojure/clojure]]])

(require
  '[provisdom.boot-tasks.core :refer :all])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom utility-belt"
       :url         "https://gitlab.com/provisdom/utility-belt"
       :scm         {:url "https://gitlab.com/provisdom/utility-belt"}
       :license     {"Provisdom" "(c) 2015 Provisdom Inc."}})