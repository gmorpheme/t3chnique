(defproject t3chnique "0.0.1-SNAPSHOT"
  :description "TADS 3 implementation"
  :url "http://dev.gmorpheme.net/t3chnique"
  :resources-path "resources"
  :main t3chnique.main
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/algo.monads "0.1.5"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/java.classpath "0.2.2"]
                 [org.clojure/tools.logging "0.2.6"]
                 [compojure "1.1.6"]
                 [ring/ring-jetty-adapter "1.2.2"]
                 [ring-middleware-format "0.3.2"]
                 [nio "1.0.2"]
                 [hiccup "1.0.5"]
                 [ch.qos.logback/logback-classic "1.1.1"]]
  :jvm-opts ["-Xms48m" "-Xmx1g" "-XX:MaxPermSize=512M" "-server"]

  :repositories [["gmorpheme-snapshots" {:url "http://dev.gmorpheme.net/artifactory/libs-snapshot"
                                         :username :env/ARTIFACTORY_USERNAME
                                         :password :env/ARTIFACTORY_PASSWORD}]]

  :deploy-repositories [["gmorpheme-snapshots"
                         {:url "http://dev.gmorpheme.net/artifactory/libs-snapshot-local"
                          :username :env/ARTIFACTORY_USERNAME
                          :password :env/ARTIFACTORY_PASSWORD}]]

  ;; add t3 tests to resources for 
  :profiles {:dev {:source-paths ["dev"]
                   :resource-paths ["t3"]
                   :dependencies [[org.clojure/tools.trace "0.7.8"]
                                  [org.clojure/tools.namespace "0.2.4"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}})
