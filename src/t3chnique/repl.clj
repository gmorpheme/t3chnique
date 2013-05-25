(ns t3chnique.repl
  (:require [t3chnique.control :as ct]
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
  (use '[t3chnique.control :as ct])
  (use '[t3chnique.server :as sv])
  (use 'clojure.repl))

(defn develop []
  (add-uses)
  (mr/autotest)
  (restart)
  (browse-url "http://localhost:8080/games"))