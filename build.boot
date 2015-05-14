(def build-deps '[[org.clojure/clojure "1.6.0"]
                  [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                  [bwo/monads "0.2.2"]
                  [org.clojure/algo.monads "0.1.5"]
                  [org.clojure/tools.cli "0.3.1"]
                  [org.clojure/java.classpath "0.2.2"]
                  [org.clojure/tools.logging "0.3.1"]
                  [com.stuartsierra/component "0.2.3"]
                  [http-kit "2.1.16"]
                  [bidi "1.18.11"]
                  [liberator "0.12.2"]
                  [compojure "1.3.3"]
                  [ring-cors "0.1.0"]
                  [ring-middleware-format "0.5.0"]
                  [nio "1.0.3"]
                  [hiccup "1.0.5"]
                  ;; for logback and groovy configuration
                  [ch.qos.logback/logback-classic "1.1.3"]
                  [org.codehaus.groovy/groovy "2.4.3"]
                  ;; plug ins / test scope
                  [midje "1.6.3"]
                  [zilti/boot-midje "0.2.1-SNAPSHOT"]
                  [boot-deps "0.1.4"]])

(def build-resources #{"src" "resources"})

(def test-deps (vec (concat build-deps '[[org.clojure/tools.trace "0.7.8"]
                                         [org.clojure/tools.namespace "0.2.10"]
                                         [ring/ring-mock "0.2.0"]])))

(def test-resources #{"src" "resources" "t3" "dev" "test"})

(set-env!
 :resource-paths test-resources
 :dependencies test-deps)

(require '[zilti.boot-midje :refer [midje]])
(require '[boot-deps :refer [ancient]])

(task-options!
 pom {:project 't3chnique
      :version "0.0.1-SNAPSHOT"}
 jar {:main 't3chnique.main}
 repl {:init-ns 'dev})

(deftask build
  "Build t3chnique engine and REST server."
  []
  (set-env! :dependencies build-deps)
  (set-env! :resources build-resources)
  (comp (pom) (jar)))
