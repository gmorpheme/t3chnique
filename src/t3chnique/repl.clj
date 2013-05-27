(ns t3chnique.repl
  (:require t3chnique.all
            [t3chnique.control :as ct]
            [t3chnique.server :as sv]
            [midje.repl :as mr])
  (:use [clojure.java.browse]))

(defonce s (atom nil))

(defn restart []
  (when-let [old @s]
    (.stop old))
  (reset! s (sv/start)))

(defn stop []
  (when-let [old @s]
    (.stop @s)
    (reset! s nil)))

(defn add-uses []
  (require '[t3chnique.control :as ct])
  (require '[t3chnique.server :as sv])
  (require '[t3chnique.server :as vm])
  (require '[t3chnique.primitive :as p])
  (require '[t3chnique.monad :as m])
  (use 'clojure.repl))

(defn develop []
  (add-uses)
  (mr/autotest)
  (restart)
  (browse-url "http://localhost:8080/games"))