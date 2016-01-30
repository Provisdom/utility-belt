(defproject provisdom/utility-belt "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.apache.commons/commons-lang3 "3.1"]
                 [provisdom/test "0.1.0" :scope "test"]
                 [midje "1.8.3" :scope "test" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-midje "3.1.3"]])
