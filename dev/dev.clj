(ns dev
  (:require [t3chnique.server :as sv]
            [t3chnique.vm :as vm]
            [t3chnique.parse :as parse]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [t3chnique.dump :refer [dump-state load-state]]
            [t3chnique.all]
            [clojure.pprint :refer (pprint pp)]
            [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [midje.repl :refer (autotest)]
            [clojure.tools.trace :refer :all]
            [clojure.java.browse :refer (browse-url)]))

(println "Starting t3chnique development REPL")

(def system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
                  (constantly (sv/system)))
  nil)

(defn start
  "Starts the current development system."
  []
  (alter-var-root #'system sv/start)
  nil)

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
