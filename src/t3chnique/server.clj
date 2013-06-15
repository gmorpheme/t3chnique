(ns ^{:doc "Server for maintaining managing running VMs."}
    t3chnique.server
  (:use [ring.middleware.format-params :only [wrap-restful-params]]
        [ring.middleware.format-response :only [wrap-format-response serializable? make-encoder]]
        [ring.middleware.stacktrace :only [wrap-stacktrace-web]]
        [compojure.core :only [routes GET POST]]
        clojure.java.io)
  (:require [ring.adapter.jetty :as j]
            [ring.util.response :as response]
            [compojure.handler :as handler]
            [cheshire.custom :as json]
            [clj-yaml.core :as yaml]
            [hiccup.core :as h]
            [hiccup.page :as hp]
            [clojure.tools.trace :as t]
            [clojure.string :as string]
            [clojure.java.classpath :as cp]
            [compojure.route :as route]
            [t3chnique.vm :as t3vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [clojure.stacktrace :as st]))

(defn vm-actions
  "Return the actions available for the vm at its current state"
  [vm]
  (if (zero? (:ip vm)) [:action/enter] [:action/step :action/run :action/back]))

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

(defn add-vm-links
  [id actions repn]
  (let [basic {:self {:href (str "/vms/" id)}
               :stack {:href (str "/vms/" id "/stack")}
               :registers {:href (str "/vms/" id "/registers")}
               :exec {:href (str "/exec/" id)}
               :code {:href (str "/vms/" id "/code{?address,length}") :templated true}
               :const {:href (str "/vms/" id "/const{?address,length") :templated true}
               :objects {:href (str "/vms/" id "/objects{?oid,count}") :templated true}
               :mcld {:href (str "/vms/" id "/mcld")}
               :fnsd {:href (str "/vms/" id "/fnsd")}
               :symd {:href (str "/vms/" id "/symd")}
               :exc {:href (str "/vms/" id "/exc")}
               :dis1 {:href (str "/vms/" id "/dis1/{address}") :templated true}}
        action-links (map (fn [x] {x {:href (str "/vms/" id "/" (name x))
                                     :name (string/capitalize (name x))}})
                          actions)
        links (reduce merge basic action-links)]
    (assoc repn :_links links)))

(defn represent-vm [id vm]
  (add-vm-links id (vm-actions vm) {:id id}))

(defn represent-vm-stack [id vm]
  (add-vm-links id (vm-actions vm) (select-keys vm [:stack])))

(defn represent-vm-registers [id vm]
  (add-vm-links id
                (vm-actions vm)
                (promote-types (select-keys vm [:r0 :ip :ep :sp :fp :say-function :say-method]))))

(defn represent-vm-map [vms]
  (for [[id vm] vms]
    (add-vm-links id (vm-actions vm) {:id id :sequence (:sequence vm)})))

(defn ubytes [page offset len]
  (map #(bit-and 0xff %) (parse/read-bytes page offset len)))

(defn represent-vm-code [id vm addr len]
  (let [[p off] (t3vm/offset vm addr)]
    (add-vm-links
     id
     (vm-actions vm)
     {:id id :code-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-assembly [id vm addr {:keys [op args]}]
  (let [doc-url (str "http://www.tads.org/t3doc/doc/techman/t3spec/opcode.htm#opc_" (.toLowerCase (name (:mnemonic op))))
        op (dissoc op :run-fn)]
    (add-vm-links id
                  (vm-actions vm)
                  {:id id
                   :assembly {:op op
                              :args args
                              :address addr
                              :_links {:doc {:href doc-url}}}})))

(defn represent-vm-const [id vm addr len]
  (let [[p off] (t3vm/const-offset vm addr)]
    (add-vm-links
     id
     (vm-actions vm)
     {:id id :const-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-objects [id vm oid count]
  (add-vm-links
   id
   (vm-actions vm)
   {:id id
    :objs (map
           (fn [[k v]] {:oid (p/vm-obj k) :value v})
           (take count (subseq (:objs vm) >= oid)))}))

(defn represent-vm-mcld [id vm]
  (add-vm-links
   id
   (vm-actions vm)
   {:id id
    :mcld (map #(dissoc % :metaclass :_prototype) (:mcld vm))}))

(defn represent-vm-fnsd [id vm]
  (add-vm-links
   id
   (vm-actions vm)
   {:id id
    :fnsd (:fnsd vm)}))

(defn represent-vm-symd [id vm]
  (add-vm-links
   id
   (vm-actions vm)
   {:id id
    :symd (:symd vm)}))

(defn represent-vm-exc [id vm]
  (add-vm-links
   id
   (vm-actions vm)
   {:id id
    :exc (or (:exc vm) nil)}))

(defn respond
  ([data]
     {:body data})
  ([page data]
     {:body (with-meta data {:page page})}))




;; tooling site - static html access to the restful data

(defn chrome [& body]
  (hp/html5
   [:html
    [:head
     [:link {:href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css" :rel "stylesheet"}]
     [:link {:href "/css/tools.css" :rel "stylesheet"}]]
    [:body
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
   [:div {:id "information" :clear "left" :width "100%"}]
   [:div {:id "controls" :clear "left" :width "100%"}]
   [:div
    [:div {:class "stack"}]
    [:div {:class "register"}]
    [:div {:class "code"}]
    [:div {:class "constant"}]
    [:div {:class "object"}]
    [:div {:class "inspector"}]]
   [:script {:type "text/javascript" :src "/vm.js"}]))


(defn scan-for-t3
  "Scan the classpath for .t3 files to produce the game catalogue."
  []
  (->> (cp/classpath-directories)
       (mapcat file-seq)
       (map #(.getName %))
       (filter #(.endsWith % ".t3"))))

(defn new-id
  "Create a new id string with prefix"
  [prefix]
  (apply str prefix \- (take 6 (->> (repeatedly (partial rand-int 26))
                                    (map (partial + 97))
                                    (map char)))))

(defn game-list
  "List available games."
  [system] (:game-catalogue system))

(defn game-get
  "Retrive game by id."
  [system id]
  (first (filter #(= (:id %) id)
                 (game-list system))))

(defn vm-map
  "List ids of all VMs in the system."
  [system]
  {:pre [(not (nil? system))]}
  (map (fn [[k v]] [k (peek v)]) @(:vm-histories system)))

(defn vm-get
  "Return a VM by id"
  [system id] (peek (get @(:vm-histories system) id)))

(defn vm-new!
  "Create a new VM for the game specified"
  [system game-id]
  (let [name (:name (game-get system game-id))
        prefix (first (string/split name #"\W"))
        histories (:vm-histories system)
        id (first (remove (partial contains? @histories) (repeatedly #(new-id prefix))))
        vm (assoc (t3vm/vm-from-image (parse/parse-resource name)) :id id)]
    (swap! histories assoc id [vm])
    vm))

(defn vm-destroy!
  "Delete a VM by id"
  [system id]
  (swap! (:vm-histories system) dissoc id)
  nil)

(defn vm-enter!
  "Enter the VM with specified id."
  [system id]
  (let [[r s] ((t3vm/enter) (vm-get system id))]
    (swap! (:vm-histories system) update-in [id] conj s)
    (vm-get system id)))

(defn vm-step!
  "Single-step the VM with specified id."
  [system id]
  (let [initial-state (vm-get system id)]
    (try
      (let [[e s] ((t3vm/step) initial-state)]
        (swap! (:vm-histories system) update-in [id] conj (assoc s :exc nil)))
      (catch Throwable t
        (swap! (:vm-histories system)
               update-in [id] conj (assoc initial-state :exc (with-out-str (st/print-stack-trace t))))))
    (vm-get system id)))

(defn vm-back!
  "Reverse the last operation."
  [system id]
  (swap! (:vm-histories system) update-in [id] pop)
  (vm-get system id))

(defn vm-run!
  "Run VM until an error is hit or input is required."
  [system id]
  (let [initial-state (vm-get system id)]
    (try
      (let [[e s] ((t3vm/run-state) (vm-get system id))]
        (swap! (:vm-histories system) update-in [id] conj (assoc s :exc nil)))
      (catch Throwable t
        (swap! (:vm-histories system) update-in [id] conj (assoc initial-state :exc (with-out-str (st/print-stack-trace t))))))
    (vm-get system id)))

(defn dis1
  "Disassemble instruction at addr and return {:op {...} :args {...}}"
  [system id addr]
  (let [vm (vm-get system id)]
    (-> (t3vm/offset vm addr)
        ((t3vm/parse-op))
        (first)
        ((fn [[op args]] {:op op :args args})))))

(defn make-routes
  "Make routes for system"
  [system]
  {:pre [(not (nil? system))
         (associative? system)]}
  (routes

   (GET "/games" []
     (respond "t3chnique.server/games-page" (represent-game-list (game-list system))))

   (GET ["/games/:id"] [id]
     (let [id (Integer/parseInt id)]
       (if-let [game (game-get system id)]
         (respond "t3chnique.server/game-page" (represent-game game))
         (response/not-found "Nonesuch"))))

   (GET "/vms" []
     (respond "t3chnique.server/vms-page" (represent-vm-map (vm-map system))))

   (GET ["/vms/:id"] [id]
     (if-let [vm (vm-get system id)]
       (respond "t3chnique.server/vm-page" (represent-vm id vm))
       (response/not-found "Nonesuch")))
   
   (GET ["/vms/:id/stack"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-stack id vm))
       (response/not-found "Nonesuch")))
   
   (GET ["/vms/:id/registers"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-registers id vm))
       (response/not-found "Nonesuch")))
   
   (GET ["/vms/:id/code"] [id address length]
     (let [addr (Integer/parseInt address)
           len (Integer/parseInt length)]
       (if-let [vm (vm-get system id)]
         (respond (represent-vm-code id vm addr len))
         (response/not-found "Nonesuch"))))

   (GET ["/vms/:id/const"] [id address length]
     (let [addr (Integer/parseInt address)
           len (Integer/parseInt length)]
       (if-let [vm (vm-get system id)]
         (respond (represent-vm-const id vm addr len))
         (response/not-found "Nonesuch"))))

   (GET ["/vms/:id/objects"] [id oid count]
     (let [o (Integer/parseInt oid)
           n (Integer/parseInt count)]
       (if-let [vm (vm-get system id)]
         (respond (represent-vm-objects id vm o n))
         (response/not-found "Nonesuch"))))
   
   (GET ["/vms/:id/mcld"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-mcld id vm))
       (response/not-found "Nonesuch")))

   (GET ["/vms/:id/fnsd"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-fnsd id vm))
       (response/not-found "Nonesuch")))
   
   (GET ["/vms/:id/symd"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-symd id vm))
       (response/not-found "Nonesuch")))
   
   (GET ["/vms/:id/exc"] [id]
     (if-let [vm (vm-get system id)]
       (respond (represent-vm-exc id vm))
       (response/not-found "Nonesuch")))

   ;; actions
   
   (POST ["/vms"] [game]
     (let [game (Integer/parseInt game)]
       (response/redirect-after-post (str "/vms/" (:id (vm-new! system game))))))
   
   (POST ["/vms/:id/step"] [id]
     (respond (represent-vm id (vm-step! system id))))
   
   (POST ["/vms/:id/enter"] [id]
     (respond (represent-vm id (vm-enter! system id))))
   
   (POST ["/vms/:id/run"] [id]
     (respond (represent-vm id (vm-run! system id))))

   (POST ["/vms/:id/back"] [id]
     (respond (represent-vm id (vm-back! system id))))
   
   (GET ["/vms/:id/dis1/:addr"] [id addr]
     (let [addr (Integer/parseInt addr)]
       (if-let [vm (vm-get system id)]
         (respond (represent-vm-assembly id (vm-get system id) addr (dis1 system id addr)))
         (response/not-found "Nonesuch"))))
   
   (GET ["/exec/:id"] [id]
     (if-let [vm (vm-get system id)]
       (respond "t3chnique.server/exec-page" (represent-vm id vm))
       (response/not-found "Nonesuch")))
   
   (route/resources "/")
   
   (route/not-found "No such resource")))

(defn make-app [system]
  (-> (handler/api (make-routes system))
      (wrap-restful-params)
      (wrap-response)
      (wrap-stacktrace-web)))

(defn start-server [system]
  (j/run-jetty (make-app system) {:port 8080 :join? false}))

(defn stop-server [system]
  (when-let [server @(:server system)]
    (.stop server)))

(defn system 
  "Initialise a new instance of the whole application."
  []
  (let [t3s (scan-for-t3)
        game-catalogue (map (fn [id name] {:id id :name name}) (range (count t3s)) t3s)]
    {:game-catalogue game-catalogue
     :vm-histories (atom {})
     :server (atom nil)}))

(defn start
  "Start an instance of the application."
  [system]
  {:pre [(not (nil? system))]}
  (swap! (:server system) (constantly (start-server system)))
  system)

(defn stop
  "Stop serving requests."
  [system]
  (stop-server system)
  system)