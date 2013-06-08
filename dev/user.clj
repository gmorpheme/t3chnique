(ns user
  (:require [t3chnique.server :as sv]
            [t3chnique.vm :as vm]
            [t3chnique.parse :as parse]
            [t3chnique.primitive :as p]
            [t3chnique.monad :as m]
            [clojure.pprint :refer (pprint pp)]
            [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [midje.repl :refer (autotest)]
            [clojure.java.browse :refer (browse-url)]))

(println "Starting t3chnique development REPL")

(def system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
    (constantly (sv/system))))

(defn start
  "Starts the current development system."
  []
  (alter-var-root #'system sv/start))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system
                  (fn [s] (when s (sv/stop s))))
  nil)

(defn go
  "Initializes the current development system and starts it running."
  []
  (init)
  (start)
  (browse-url "http://localhost:8080/games"))

(defn reset []
  (stop)
  (refresh :after 'user/go))