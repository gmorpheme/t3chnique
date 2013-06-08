(ns t3chnique.repl
  (:require t3chnique.all
            [t3chnique.control :as ct]
            [t3chnique.server :as sv]
            [midje.repl :as mr])
  (:use [clojure.java.browse]
        [clojure.java.classpath]))

(defn scan-for-t3 []
  (->> (classpath-directories)
       (mapcat file-seq)
       (map #(.getName %))
       (filter #(.endsWith % ".t3"))))

(alter-var-root
 #'ct/game-catalogue
 (let [t3s (scan-for-t3)]
   (constantly
    (map
     (fn [id name] {:id id :name name})
     (range (count t3s))
     t3s))))

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
  (require '[t3chnique.parse :as parse])
  (require '[t3chnique.primitive :as p])
  (require '[t3chnique.monad :as m])
  (use 'clojure.repl))

(defn develop []
  (add-uses)
  (mr/autotest)
  (restart)
  (browse-url "http://localhost:8080/games"))

