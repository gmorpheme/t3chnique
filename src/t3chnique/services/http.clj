(ns t3chnique.services.http
  (:require [org.httpkit.server :as http]
            [com.stuartsierra.component :as component]
            [bidi.ring :refer (make-handler)]
            [t3chnique.services.webmount :as webmount]
            [clojure.tools.logging :refer (info error)]))

(defn merge-mounts
  "Merge together a map of named routes into a single bidi route definition."
  [route-map]
  (into {} (map (fn [[k v]] [(str k (first v)) (second v)]) route-map)))

(defn wrap-logging
  "Simple logging middleware."
  [app]
  (fn [req]
    (try
      (info "REQ:" req)
      (let [resp ((app req) req)]
        (info "RSP: " (:status resp))
        resp)
      (catch Exception e
        (error "Error during request:" e)))))

(defrecord HttpServer [vm-store game-catalogue server routes handler]

  component/Lifecycle
  (start [self]
    (let [routes ["/" (merge-mounts {"games" (webmount/routes game-catalogue)
                                     "vms" (webmount/routes vm-store)})]
          handler (wrap-logging
                   (make-handler routes
                                 (merge (webmount/handlers game-catalogue)
                                        (webmount/handlers vm-store))))]
      (assoc self
             :routes routes
             :handler handler
             :server (http/run-server handler {}))))

  (stop [self]
    (server :timeout 100)
    (assoc self :server nil)))

(defn create-server []
  (map->HttpServer {}))
