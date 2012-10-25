(ns t3chnique.server
  (:use compojure.core
        ring.adapter.jetty
        ring.middleware.edn
        [compojure.handler :as handler]
        clojure.java.io
        [t3chnique.image :only [parse-image load-image-file]]
        hiccup.core)
  (:require [compojure.route :as route]))

(defn edn-response
  "Render data as edn"
  [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "text/edn"}
   :body (pr-str data)})

(defn render-html [data]
  (html [:body (pr-str data)]))

(defn html-response
  "Render as html report"
  [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "text/html"}
   :body (render-html data)})

(def game-catalogue [{:id 1 :name "Elysium.t3"}
                     {:id 2 :name "ditch.t3"}])

(defn game-list [] game-catalogue)
(defn game-get [id] (first (filter #(= (:id %) (int id)) game-catalogue)))

(def vms (atom {}))

(defn vm-list [] @vms)
(defn vm-get [id] (first (filter #(= (:id %) (int id)) @vms)))
(defn vm-new [game]
  (let [f (file (resource (:name (game-get game))))
        buf (load-image-file f)
        blocks (parse-image buf)
        id (inc (count @vms)) ;; concurrency
        vm {:id id :blocks blocks}]
    (swap! vms conj vm)
    (vm-get id)))

(defroutes vm-routes

  (GET "/games" []
       (edn-response (game-list)))
  (GET "/games/:id" [id]
       (edn-response (game-get id)))

  (GET "/vms" []
       (edn-response (vm-list)))
  (GET "/vms/:id" [id]
       (edn-response (vm-get id)))
  (POST "/vms" [game]
        (println "new vm " game)
        (edn-response (vm-new game))))


(def app
  (-> (handler/api vm-routes)
      wrap-edn-params))

(defn main []
  (run-jetty #'app {:port 8080 :join? false}))