(ns t3chnique.server
  (:use compojure.core
        ring.adapter.jetty
        [compojure.handler :as handler]
        [t3chnique.image :only [parse-image load-image-file]]
        [ring.middleware.format-params :only [wrap-restful-params]]
        [ring.middleware.format-response :only [wrap-restful-response]]
        [ring.middleware.stacktrace :only [wrap-stacktrace-web]]
        clojure.java.io
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

;; state and operations

(def game-catalogue [{:id 1 :name "Elysium.t3"}
                     {:id 2 :name "ditch.t3"}])

(defn game-list [] game-catalogue)
(defn game-get [id] (first (filter #(= (:id %) id) game-catalogue)))

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

(defn respond [data]
  {:body data})

(defroutes vm-routes
  (GET "/games" []
       (respond (game-list)))
  (GET "/games/:id" [id]
       (respond (game-get (Integer/parseInt id))))
  (GET "/vms" []
       (respond (vm-list)))
  (GET "/vms/:id" [id]
       (respond (vm-get id)))
  (POST "/vms" [game]
        (respond (vm-new game)))
  (GET "/vms/:id/code/:block"
       )
  
  )

(def app
  (-> (handler/api vm-routes)
      (wrap-restful-params)
      (wrap-restful-response)
      (wrap-stacktrace-web)))

(defn start []
  (run-jetty #'app {:port 8080 :join? false}))