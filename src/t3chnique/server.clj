(ns t3chnique.server
  (:use [t3chnique.image :only [parse-image load-image-file parse-resource]]
        [ring.middleware.format-params :only [wrap-restful-params]]
        [ring.middleware.format-response :only [wrap-format-response serializable? make-encoder]]
        [ring.middleware.stacktrace :only [wrap-stacktrace-web]]
        clojure.java.io)
  (:require [compojure.core :as c]
            [ring.adapter.jetty :as j]
            [compojure.handler :as handler]
            [cheshire.custom :as json]
            [clj-yaml.core :as yaml]        
            [hiccup.core :as h])
  (:require [compojure.route :as route]
            [t3chnique.vm :as t3vm]))

(defn render-html [data]
  (if-let [page (:page (meta data))]
    (h/html [:html [:body  (str (resolve *ns* page))]])
    (h/html [:html [:body (pr-str data)]])))

(defn render-edn [data]
  (binding [*print-dup* true]
    (pr-str data)))

;; middleware for returning json, edn or html based on accept header
(defn wrap-response [handler]
  (wrap-format-response handler
                        :predicate serializable?
                        :encoders [(make-encoder json/generate-string "application/json")
                                   (make-encoder render-edn "application/edn")
                                   (make-encoder yaml/generate-string "application/x-yaml")
                                   (make-encoder render-html "text/html")]
                        :charset "utf-8"))

;; state and operations

(def game-catalogue [{:id 1 :name "Elysium.t3"}
                     {:id 2 :name "ditch.t3"}])

(defn game-list [] game-catalogue)
(defn game-get [id] (first (filter #(= (:id %) id) game-catalogue)))

(def vms (atom {}))

(defn vm-list [] @vms)
(defn vm-get [id] (first (filter #(= (:id %) (int id)) @vms)))
(defn vm-new [game]
  (let [name  (:name (game-get game))
        vm (t3vm/vm-from-image (parse-resource name))
        id (inc (count @vms)) ;; concurrency
        vm (assoc vm :id id)]
    (swap! vms conj vm)
    (vm-get id)))

(defn respond
  ([data]
     {:body data})
  ([page data]
     {:body (with-meta data {:page page})}))

(c/defroutes vm-routes
  (GET "/games" []
       (respond 'games-page (game-list)))
  (GET "/games/:id" [id]
       (respond (game-get (Integer/parseInt id))))
  (GET "/vms" []
       (respond (vm-list)))
  (GET "/vms/:id" [id]
       (respond (vm-get id)))
  (POST "/vms" [game]
        (respond (vm-new game))))

(def app
  (-> (handler/api vm-routes)
      (wrap-restful-params)
      (wrap-response)
      (wrap-stacktrace-web)))

(defn start []
  (j/run-jetty #'app {:port 8080 :join? false}))

(defn game-page [games]
  (h/html [:html [:body (for [game games] [:em (:name game)])]]))