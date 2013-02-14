(defproject t3chnique "0.0.1-SNAPSHOT"
  :description "TADS 3 implementation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resources-path "resources"
  :main t3chnique.main
  :warn-on-reflection true
  :jvm-opts ["-Xmx1g" "-Xshare:off" "-agentpath:C:/Program Files/YourKit Java Profiler 12.0.3/bin/win32/yjpagent.dll=sampling"]
  :dependencies [[org.clojure/clojure "1.5.0-RC4"]
                 [org.clojure/algo.monads "0.1.0"]
                 [nio "0.0.3"]
                 [compojure "1.1.3"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [ring-middleware-format "0.2.2"]
                 [hiccup "1.0.2"]
                 [org.clojure/tools.trace "0.7.3"]
                 [midje "1.4.0"]
                 [org.clojure/tools.cli "0.2.2"]])
