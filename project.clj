(defproject t3chnique "0.0.1-SNAPSHOT"
  :description "TADS 3 implementation"
  :url "http://dev.gmorpheme.net/t3chnique"
  :resources-path "resources"
  :main t3chnique.main
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/algo.monads "0.1.4"]
                 [org.clojure/tools.cli "0.2.4"]
                 [org.clojure/java.classpath "0.2.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [compojure "1.1.6"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [ring-middleware-format "0.3.1"]
                 [nio "1.0.1"]
                 [hiccup "1.0.4"]]
  :jvm-opts ["-Xms48m" "-Xmx1g" "-XX:MaxPermSize=512M" "-server"]

  :repositories [["gmorpheme-snapshots" {:url "http://dev.gmorpheme.net/artifactory/libs-snapshot"}]]
  :deploy-repositories [["gmorpheme-snapshots" {:url "http://dev.gmorpheme.net/artifactory/libs-snapshot-local"}]]
  
  ; add t3 tests to resources for 

  :profiles {:dev {:source-paths ["dev"]
                   :resource-paths ["t3"]
                   :dependencies [[org.clojure/tools.trace "0.7.6"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [midje "1.5.1"]]
                   :plugins [[lein-midje "3.0.0"]]}})
