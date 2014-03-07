(defproject bartok "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0-beta1"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [org.clojure/core.match "0.2.1"]
                 [overtone/at-at "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.codehaus.jsr166-mirror/jsr166y "1.7.0"]
                 [net.mikera/clisk "0.8.0"]
                 
                 [reiddraper/simple-check "0.5.6"]
                 [clocop "0.2.0"]
                 [camel-snake-kebab "0.1.2"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/data.zip "0.1.1"]
                 [into-edn "1.0.2"]
                 [instaparse "1.2.16"]
                 
                 [org.clojure/core.contracts "0.0.5"]
                 [org.clojure/tools.reader "0.8.3"]
                 [org.clojure/java.data "0.1.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/algo.monads "0.1.4"]]
  :profiles {:dev {:dependencies [[midje "1.6.2"]]}}
  ;:repl-options {:nrepl-middleware [utils.nrepl-middleware/pprint-middleware]}
  ; :main bartok.core
  :java-source-paths ["src/java/"]
  :javac-target "1.7.0_45")
