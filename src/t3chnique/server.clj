(ns t3chnique.server
  (:use [t3chnique.parse :only [load-image-file parse-resource]]
        [ring.middleware.format-params :only [wrap-restful-params]]
        [ring.middleware.format-response :only [wrap-format-response serializable? make-encoder]]
        [ring.middleware.stacktrace :only [wrap-stacktrace-web]]
        [compojure.core :only [defroutes GET POST]]
        clojure.java.io)
  (:require [ring.adapter.jetty :as j]
            [ring.util.response :as response]
            [compojure.handler :as handler]
            [cheshire.custom :as json]
            [clj-yaml.core :as yaml]        
            [hiccup.core :as h]
            [hiccup.page :as hp]
            [clojure.tools.trace :as t])
  (:require [compojure.route :as route]
            [t3chnique.vm :as t3vm]))

(defn render-html [data]
  (if-let [page (:page (meta data))]
    ((resolve (symbol page)) data)
    (hp/html5 (pr-str data))))

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
                     {:id 2 :name "ditch3.t3"}])

(defn game-list [] game-catalogue)
(defn game-get [id] (first (filter #(= (:id %) id) game-catalogue)))

(def vms (atom {}))

(defn vm-map [] @vms)
(defn vm-get [id] (get @vms (int id)))
(defn vm-new [game]
  (let [name  (:name (game-get game))
        vm (t3vm/vm-from-image (parse-resource name))
        id (inc (count @vms)) ;; concurrency
        vm (assoc vm :id id)]
    (swap! vms assoc id vm)
    (vm-get id)))

;; represent functions add hyperlinks using HAL to support HATEOAS
;; style interaction

(defn represent-game [game]
  (assoc game :_links {:self {:href (str "/games/" (:id game))}}))

(defn represent-game-list [gs]
  (map represent-game gs))

(defn add-vm-links [id vm]
  (assoc vm :_links {:self {:href (str "/vms/" id)}
                     :stack {:href (str "/vms/" id "/stack")}
                     :registers {:href (str "/vms/" id "/stack")}}))

(defn represent-vm [id vm]
  (add-vm-links id {:id id}))

(defn represent-vm-stack [id vm]
  (add-vm-links id (:stack vm)))

(defn represent-vm-registers [id vm]
  (add-vm-links id (select-keys [:r0 :ip :ep :sp :fp :say-function :say-method] vm)))

(defn represent-vm-map [vms]
  (for [[id vm] vms]
    (add-vm-links {:id id})))

(defn respond
  ([data]
     {:body data})
  ([page data]
     {:body (with-meta data {:page page})}))

(defroutes vm-routes
  (GET "/games" []
    (respond "t3chnique.server/games-page" (represent-game-list (game-list))))
  (GET "/games/:id" [id]
    (respond "t3chnique.server/game-page" (represent-game (game-get (Integer/parseInt id)))))
  (GET "/vms" []
    (respond "t3chnique.server/vms-page" (vm-map)))
  (GET "/vms/:id" [id]
    (respond (represent-vm id (vm-get (Integer/parseInt id)))))
  (GET "/vms/:id/stack" [id]
    (respond (represent-vm-stack id (vm-get (Integer/parseInt id)))))
  (GET "/vms/:id/registers" [id]
    (respond (represent-vm-registers id (vm-get (Integer/parseInt id)))))
  (POST "/vms" [game]
    (response/redirect-after-post (str "/vms/" (:id (vm-new (Integer/parseInt game))))))
  (route/resources "/"))

(def app
  (-> (handler/api vm-routes)
      (wrap-restful-params)
      (wrap-response)
      (wrap-stacktrace-web)))

(defn start []
  (j/run-jetty #'app {:port 8080 :join? false}))

;; tooling site - static html access to the restful data

(defn tooling-chrome [nav-name body]
  (hp/html5
   [:html
    [:head
     [:link {:href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/css/bootstrap-combined.min.css" :rel "stylesheet"}]
     [:link {:href "/css/tools.css" :rel "stylesheet"}]]
    [:body
     [:ul.nav.nav-tabs
      [:li {:class (if (= nav-name "games") "active" "inactive")} [:a {:href "/games"} "Games"]]
      [:li {:class (if (= nav-name "vms") "active" "inactive")} [:a {:href "/vms"} "VMs"]]]
     [:div.tab-content
      body]
     [:script {:src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js"}]]]))

(defn games-page [games]
  (tooling-chrome "games" [:div#games.row
                           [:div.span4
                            [:ul.nav.nav-list
                             (for [game games]
                               [:li [:a {:href (str "/games/" (:id game)) } (:name game)]])]]]))

(defn game-page [game]
  (tooling-chrome "games" [:div#game
                           [:div.span4
                            [:h2 (:name game)]
                            [:form {:method "post" :action "/vms"}
                             [:input {:type "hidden" :name "game" :value (:id game)}]
                             [:button.btn {:type "submit"} "Launch"]]]] ))

(defn vms-page [vms]
  (tooling-chrome "vms" [:div#vms.row
                         [:dev.span4
                          [:ul.nav.nav-list
                           (for [{:keys [id links]} vms]
                             [:li id
                              (for [[rel attrs] links]
                                [:a (assoc attrs :rel rel) rel])])]]]))

