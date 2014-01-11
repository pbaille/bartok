(defproject bartok "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0-alpha3"]
                 [overtone/at-at "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.0"]]}}
  ;:repl-options {:nrepl-middleware [utils.nrepl-middleware/pprint-middleware]}
  :main bartok.core
  :java-source-paths ["src/java/"])
