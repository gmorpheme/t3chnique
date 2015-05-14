(ns ^{:doc "Game catalogue microservice"}
  t3chnique.services.game-catalogue
  (:require [com.stuartsierra.component :as component]
            [clojure.java.classpath :as cp]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer (info spy)]
            [liberator.core :refer (resource)]
            [liberator.representation :refer (as-response)]
            [bidi.bidi :refer (match-route)]
            [bidi.ring :refer (make-handler)]
            [t3chnique.services.webmount :as webmount]))

(defn scan-for-t3
  "Scan the classpath for .t3 files to produce the game catalogue."
  []
  (info "Scanning for t3 resources.")
  (->> (cp/classpath-directories)
       (mapcat file-seq)
       (map #(.getName %))
       (filter #(.endsWith % ".t3"))))

(defn list-games
  "List all the available games."
  [{:keys [games]}]
  @games)

(defn get-game
  "Retrieve game by id"
  [catalogue id]
  (first (filter #(= (:id %) id)
                 (list-games catalogue))))

(defn get-image
  "Download game image bytes as t3 data. Currently assumes name
  identifies resource on classpath."
  [store id]
  (slurp (io/resource (:name (get-game store id)))))

;;;;;;;;;;;;;;
;; REST API ;;
;;;;;;;;;;;;;;

(defn game-list-resource
  "Resource to return all available games."
  [catalogue]
  (fn [{:keys [params] :as request}]
    (info "game-list-resource" request)
    (resource
     :available-media-types ["application/json" "application/edn"]
     :allowed-methods [:get]
     :handle-ok (fn [_] (list-games catalogue)))))

(defn game-resource
  "Resource which returns game by ID."
  [catalogue]
  (fn [{:keys [params] :as request}]
    (resource
     :available-media-types ["application/json" "application/edn"]
     :allowed-methods [:get]
     :exists? (fn [_]
                (when-let [game (get-game catalogue (Integer/parseInt (:id params)))]
                  {::game game}))
     :handle-ok ::game)))

(defn image-resource
  "Resource to return image content for game."
  [catalogue]
  (fn [{:keys [params]}]
    (resource
     :available-media-types ["application/x-t3vm-image"]
     :allowed-methods [:get]
     :exists? (fn [_]
                (when-let [image (get-image catalogue (Integer/parseInt (:id params)))]
                  {::image image
                   ::name (or (:name (get-game catalogue (Integer/parseInt (:id params))))
                              "game.t3")}))
     :handle-ok ::image
     :as-response (fn [d ctx]
                 (-> (as-response d ctx)
                     (assoc-in [:headers "Content-Disposition"] (str "attachment; filename='" (::name ctx) "'")))))))

(def routes
  ["/" {"" :game-list-resource
        [:id] {"" :game-resource
               "/image" :image-resource}}])

(defn handlers [catalogue]
  {:game-list-resource (game-list-resource catalogue)
   :game-resource (game-resource catalogue)
   :image-resource (image-resource catalogue)})

;; 
(defrecord GameCatalogue [games]

  component/Lifecycle

  (start [self]
    (let [t3s (scan-for-t3)]
      (swap! games into (map (fn [id name] {:id id :name name}) (range) t3s))
      self))

  (stop [self]
    self)
  
  webmount/WebMount

  (routes [self]
    routes)

  (handlers [self]
    (handlers self)))

(defn create-game-catalogue
  "Create game catalogue"
  []
  (GameCatalogue. (atom [])))
