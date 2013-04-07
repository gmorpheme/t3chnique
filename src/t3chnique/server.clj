(ns t3chnique.server
  (:use [t3chnique.parse :only [load-image-file parse-resource read-bytes]]
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
            [t3chnique.vm :as t3vm]
            [t3chnique.primitive :as p]))

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

(defn promote-types
  "Where the VM uses plain ints, promote to typed values for representation"
  [vm]
  (-> vm
      (update-in [:ep] p/vm-codeptr)
      (update-in [:sp] p/vm-stack)
      (update-in [:ip] p/vm-codeptr)
      (update-in [:fp] p/vm-stack)))

(defn represent-game [game]
  (assoc game :_links {:self {:href (str "/games/" (:id game))}}))

(defn represent-game-list [gs]
  (map represent-game gs))

(defn add-vm-links [id vm]
  (assoc vm
    :_links
    {:self {:href (str "/vms/" id)}
     :stack {:href (str "/vms/" id "/stack")}
     :registers {:href (str "/vms/" id "/registers")}
     :exec {:href (str "/exec/" id)}
     :code {:href (str "/vms/" id "/code{?address,length}") :template true}
     :const {:href (str "/vms/" id "/const{?address,length") :template true}
     :objects {:href (str "/vms/" id "/objects{?oid,count}") :template true}}))

(defn represent-vm [id vm]
  (add-vm-links id {:id id}))

(defn represent-vm-stack [id vm]
  (add-vm-links id (select-keys vm [:stack])))

(defn represent-vm-registers [id vm]
  (add-vm-links id
                (promote-types (select-keys vm [:r0 :ip :ep :sp :fp :say-function :say-method]))))

(defn represent-vm-map [vms]
  (for [[id vm] vms]
    (add-vm-links id {:id id})))

(defn ubytes [page offset len]
  (map #(bit-and 0xff %) (read-bytes page offset len)))

(defn represent-vm-code [id vm addr len]
  (let [[p off] (t3vm/offset vm addr)]
    (add-vm-links
     id
     {:id id :code-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-const [id vm addr len]
  (let [[p off] (t3vm/const-offset vm addr)]
    (add-vm-links
     id
     {:id id :const-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-objects [id vm oid count]
  (add-vm-links
   id
   {:id id
    :objs (map
           (fn [[k v]] {:oid (p/vm-obj k) :value v})
           (take count (subseq (:objs vm) >= oid)))}))

(defn respond
  ([data]
     {:body data})
  ([page data]
     {:body (with-meta data {:page page})}))

(defroutes vm-routes
  (GET "/games" []
    (respond "t3chnique.server/games-page" (represent-game-list (game-list))))
  (GET ["/games/:id" :id #"[0-9]+"] [id]
    (let [id (Integer/parseInt id)]
      (if-let [game (game-get id)]
        (respond "t3chnique.server/game-page" (represent-game game))
        (response/not-found "Nonesuch"))))
  (GET "/vms" []
    (respond "t3chnique.server/vms-page" (represent-vm-map (vm-map))))
  (GET ["/vms/:id" :id #"[0-9]+"] [id]
    (let [id (Integer/parseInt id)]
      (if-let [vm (vm-get id)]
        (respond "t3chnique.server/vm-page" (represent-vm id vm))
        (response/not-found "Nonesuch"))))
  (GET ["/vms/:id/stack" :id #"[0-9]+"] [id]
    (let [id (Integer/parseInt id)]
      (if-let [vm (vm-get id)]
        (respond (represent-vm-stack id vm))
        (response/not-found "Nonesuch"))))
  (GET ["/vms/:id/registers" :id #"[0-9]+"] [id]
    (let [id (Integer/parseInt id)]
      (if-let [vm (vm-get id)]
        (respond (represent-vm-registers id vm))
        (response/not-found "Nonesuch"))))
  (GET ["/vms/:id/code" :id #"[0-9]+"] [id address length]
    (let [id (Integer/parseInt id)
          addr (Integer/parseInt address)
          len (Integer/parseInt length)]
      (if-let [vm (vm-get id)]
        (respond (represent-vm-code id vm addr len))
        (response/not-found "Nonesuch"))))
  (GET ["/vms/:id/const" :id #"[0-9]+"] [id address length]
    (let [id (Integer/parseInt id)
          addr (Integer/parseInt address)
          len (Integer/parseInt length)]
      (if-let [vm (vm-get id)]
        (respond (represent-vm-const id vm addr len))
        (response/not-found "Nonesuch"))))
  (GET ["/vms/:id/objects" :id #"[0-9]+"] [id oid count]
    (let [id (Integer/parseInt id)
          o (Integer/parseInt oid)
          n (Integer/parseInt count)]
      (if-let [vm (vm-get id)]
        (respond (represent-vm-objects id vm o n))
        (response/not-found "Nonesuch"))))
  (POST ["/vms" :id #"[0-9]+"] [game]
    (let [game (Integer/parseInt game)]
      (response/redirect-after-post (str "/vms/" (:id (vm-new game))))))
  (GET ["/exec/:id" :id #"[0-9]+"] [id]
    (let [id (Integer/parseInt id)]
      (if-let [vm (vm-get id)]
        (respond "t3chnique.server/exec-page" (represent-vm id vm))
        (response/not-found "Nonesuch"))))
  (route/resources "/")
  (route/not-found "No such resource"))

(def app
  (-> (handler/api vm-routes)
      (wrap-restful-params)
      (wrap-response)
      (wrap-stacktrace-web)))

(defn start []
  (j/run-jetty #'app {:port 8080 :join? false}))

;; tooling site - static html access to the restful data

(defn chrome [& body]
  (hp/html5
   [:html
    [:head
     [:link {:href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/css/bootstrap-combined.min.css" :rel "stylesheet"}]
     [:link {:href "/css/tools.css" :rel "stylesheet"}]]
    [:body
     [:script {:src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/js/bootstrap.min.js"}]
     [:script {:src "http://d3js.org/d3.v3.min.js"}]
     [:script {:src "http://underscorejs.org/underscore-min.js"}]
     body]]))

(defn rest-chrome [nav-name body]
  (chrome
   [:ul.nav.nav-tabs
    [:li {:class (if (= nav-name "games") "active" "inactive")} [:a {:href "/games"} "Games"]]
    [:li {:class (if (= nav-name "vms") "active" "inactive")} [:a {:href "/vms"} "VMs"]]]
   [:div.tab-content
    body]))

(defn games-page [games]
  (rest-chrome "games" [:div#games.row
                           [:div.span4
                            [:ul.nav.nav-list
                             (for [game games]
                               [:li [:a {:href (str "/games/" (:id game)) } (:name game)]])]]]))

(defn game-page [game]
  (rest-chrome "games" [:div#game
                           [:div.span4
                            [:h2 (:name game)]
                            [:form {:method "post" :action "/vms"}
                             [:input {:type "hidden" :name "game" :value (:id game)}]
                             [:button.btn {:type "submit"} "Launch"]]]] ))

(defn vms-page [vms]
  (rest-chrome "vms" [:div#vms.row
                         [:dev.span4
                          [:ul.nav.nav-list
                           (for [{:keys [id _links]} vms]
                             [:li id
                              (for [[rel attrs] _links]
                                [:a (assoc attrs :rel rel) rel])])]]]))

(defn vm-page [{:keys [id _links]}]
  (rest-chrome "vms" [:div#vms.row
                         [:dev.span4
                          [:ul.nav.nav-list
                           [:span id]
                           (for [[rel attrs] _links]
                             [:a (assoc attrs :rel rel) rel])]]]))

(defn exec-page [{:keys [id]}]
  (chrome
   [:script (str "vm_url = '/exec/" id  "';")]
   [:div {:id "header"}]
   [:div {:id "controls" :clear "left" :width "100%"}]
   [:div
    [:div {:class "stack"}]
    [:div {:class "register"}]
    [:div {:class "code"}]
    [:div {:class "constant"}]
    [:div {:class "object"}]]
   [:script {:type "text/javascript" :src "/vm.js"}]))