(def project 'provisdom/utility-belt)
(def version "0.2.0-alpha9")

(set-env!
  :resource-paths #{"src"}
  :source-paths #{"test"}
  :dependencies '[[org.clojure/tools.nrepl "0.2.13" :scope "test"]
                  [org.clojure/test.check "0.10.0-alpha2"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [midje "1.9.2-alpha3" :exclusions [org.clojure/clojure] :scope "test"]
                  [criterium "0.4.4" :scope "test"]
                  [provisdom/boot-tasks "1.4" :scope "tests"]
                  [provisdom/test "0.3.5" :scope "test"]

                  ;;project deps
                  [org.clojure/clojure "1.9.0" :scope "provided"]
                  [org.clojure/spec.alpha "0.1.143"]
                  [orchestra "2017.11.12-1"]
                  [org.clojure/core.async "0.4.474"]
                  [org.apache.commons/commons-lang3 "3.7"]])

(require
  '[adzerk.boot-test :refer [test]]
  '[provisdom.boot-tasks.core :refer [build push-jar]])

(task-options!
  pom {:project     project
       :version     version
       :description "Provisdom utility-belt"
       :url         "https://gitlab.com/provisdom/utility-belt"
       :scm         {:url "https://gitlab.com/provisdom/utility-belt"}
       :license     {"Provisdom" "(c) 2015-2018 Provisdom Corporation"}}
  test {:namespaces '#{
                       provisdom.utility-belt.anomalies-test
                       provisdom.utility-belt.arities-test
                       provisdom.utility-belt.async-test
                       provisdom.utility-belt.debug-test
                       provisdom.utility-belt.extensions-test
                       provisdom.utility-belt.maps-test
                       provisdom.utility-belt.nils-test
                       provisdom.utility-belt.strings-test
                       }})