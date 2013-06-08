(defproject t3chnique "0.0.1-SNAPSHOT"
  :description "TADS 3 implementation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resources-path "resources"
  :main t3chnique.main
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/tools.logging "0.2.6"]
                 [compojure "1.1.5"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring-middleware-format "0.3.0"]
                 [nio "0.0.5"]
                 [hiccup "1.0.3"]]
  :jvm-opts ["-Xms48m" "-Xmx1024m" "-XX:MaxPermSize=512m"]

  ; add t3 tests to resources for 
  :profiles {:dev {:source-paths ["dev"]
                   :resource-paths ["t3"]
                   :dependencies [[org.clojure/tools.trace "0.7.5"]
                                  [midje "1.5.1"]]}})
