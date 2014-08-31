(ns ^{:doc "Server for maintaining managing running VMs."}
    t3chnique.server
  (:use [ring.middleware.format-params :only [wrap-restful-params]]
        [ring.middleware.format-response :only [wrap-format-response serializable? make-encoder]]
        [ring.middleware.stacktrace :only [wrap-stacktrace-web]]
        [compojure.core :only [routes GET POST DELETE]]
        clojure.java.io)
  (:require [ring.adapter.jetty :as j]
            [ring.util.response :as response]
            [compojure.handler :as handler]
            [cheshire.custom :as json]
            [clj-yaml.core :as yaml]
            [hiccup.core :as h]
            [hiccup.page :as hp]
            [clojure.string :as string]
            [clojure.java.classpath :as cp]
            [compojure.route :as route]
            [t3chnique.vm :as t3vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [t3chnique.inspect :as i]
            [clojure.stacktrace :as st]
            [clojure.tools.logging :refer (info debug debugf trace error spy)]
            [t3chnique.all :refer [default-host]]))

(defn pre-trace-step [op args ip]
  (trace "@" ip ":" (:mnemonic op)))

(defn vm-actions
  "Return the actions available for the vm at its current state"
  [vm]
  (if (zero? (:ip vm)) [:action/enter] [:action/step :action/run :action/back]))

(defn render-html [data]
  (trace "render-html")
  (if-let [{:keys [page context]} (meta data)]
    ((resolve (symbol page)) context data)
    (hp/html5 (pr-str data))))

(defn render-edn [data]
  (trace "render-edn")
  (pr-str data))

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

(defn url
  "Form a URL with the supplied context root and path elements. context should
start with a /. Use / for the root context."
  [context & components]
  (let [path (string/join "/" components)]
    (if (string/blank? context)
      path
      (str context path))))

(defn represent-game [context game]
  (assoc game :_links {:self {:href (url context "games" (:id game))}
                       :launch {:href (url context "vms")}}))

(defn represent-game-list [context gs]
  {:games (map (partial represent-game context) gs)})

(defn add-vm-links
  "Add HATEOAS style links for further information / functionality."
  [context id actions repn]
  (let [basic {:self {:href (url context "vms" id)
                      :doc "GET for basic details. DELETE to destroy."}
               
               :stack {:href (url context "vms" id "stack")}
               
               :registers {:href (url context "vms" id "registers")}
               
               :exec {:href (url context "exec" id)}
               
               :code {:href (url context "vms" id "code{?address,length}")
                      :templated true
                      :doc "Return section of code pool of specified length, starting from address."}
               
               :const {:href (url context "vms" id "const{?address,length}")
                       :templated true
                       :doc "Return section of const pool of specified length, starting from address."}
               
               :objects {:href (url context "vms" id "objects{?oid,count}")
                         :templated true
                         :doc "List count objects starting at oid."}
               
               :mcld {:href (url context "vms" id "mcld")}
               
               :fnsd {:href (url context "vms" id "fnsd")}
               
               :symd {:href (url context "vms" id "symd")}
               
               :exc {:href (url context "vms" id "exc")}
               
               :dis1 {:href (url context "vms" id "dis1" "{address}") :templated true}
               
               :inspect-state {:href (url context "vms" id "inspect-state")}}
        
        action-links (map (fn [x] {x {:href (url context "vms" id (name x))
                                     :name (string/capitalize (name x))
                                     :method :post}})
                          actions)
        links (reduce merge basic action-links)]
    (assoc repn :_links links)))

(defn represent-vm [context id vm]
  (add-vm-links context id (vm-actions vm) {:id id :sequence (:sequence vm)}))

(defn represent-vm-stack [context id vm]
  (add-vm-links context id (vm-actions vm) (select-keys vm [:stack])))

(defn represent-vm-registers [context id vm]
  (add-vm-links context
                id
                (vm-actions vm)
                (promote-types (select-keys vm [:r0 :ip :ep :sp :fp :say-function :say-method]))))

(defn represent-vm-map [context vms]
  (for [[id vm] vms]
    (add-vm-links context id (vm-actions vm) {:id id :sequence (:sequence vm)})))

(defn ubytes [page offset len]
  (map #(bit-and 0xff %) (parse/read-bytes page offset len)))

(defn represent-vm-code [context id vm addr len]
  (let [[p off] (t3vm/offset vm addr)]
    (add-vm-links
     context
     id
     (vm-actions vm)
     {:id id :code-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-assembly [context id vm addr {:keys [op args]}]
  (let [doc-url (str "http://www.tads.org/t3doc/doc/techman/t3spec/opcode.htm#opc_" (.toLowerCase (name (:mnemonic op))))
        op (dissoc op :run-fn)]
    (add-vm-links context
                  id
                  (vm-actions vm)
                  {:id id
                   :assembly {:op op
                              :args args
                              :address addr
                              :_links {:doc {:href doc-url}}}})))

(defn represent-vm-const [context id vm addr len]
  (let [[p off] (t3vm/const-offset vm addr)]
    (add-vm-links
     context
     id
     (vm-actions vm)
     {:id id :const-section {:address addr :bytes (ubytes p off len)}})))

(defn represent-vm-objects [context id vm oid count]
  (add-vm-links
   context
   id
   (vm-actions vm)
   {:id id
    :objs (map
           (fn [[k v]] {:oid (p/vm-obj k) :value v})
           (take count (subseq (:objs vm) >= oid)))}))

(defn represent-vm-mcld [context id vm]
  (add-vm-links
   context
   id
   (vm-actions vm)
   {:id id
    :mcld (map #(dissoc % :metaclass :_prototype) (:mcld vm))}))

(defn represent-vm-fnsd [context id vm]
  (add-vm-links
   context
   id
   (vm-actions vm)
   {:id id
    :fnsd (:fnsd vm)}))

(defn represent-vm-symd [context id vm]
  (add-vm-links
   context
   id
   (vm-actions vm)
   {:id id
    :symd (:symd vm)}))

(defn represent-vm-exc [context id vm]
  (add-vm-links
   context
   id
   (vm-actions vm)
   {:id id
    :exc (or (:exc vm) nil)}))

(defn represent-vm-annotated-state
  "Represent the VM as its registers and stack, annotated with
useful information for tooling UIs."
  [context id vm]
  (let [annotate-registers #(reduce
                             (fn [s k] (update-in s [k] (partial i/annotate-primitive %)))
                             %
                             [:r0 :ip :ep :sp :fp :say-function :say-method])
        annotate-stack  #(reduce
                          (fn [s i] (update-in s [:stack i] (partial i/annotate-primitive %)))
                          %
                          (range (p/value (:sp %))))
        add-links (partial add-vm-links context id (vm-actions vm))]
    (-> vm
        (promote-types)
        (annotate-registers)
        (annotate-stack)
        (select-keys [:r0 :ip :ep :sp :fp :say-function :say-method :stack])
        (add-links))))

(defn respond
  ([context data]
     {:body data})
  ([context page data]
     {:body (with-meta data {:page page :context context})}))

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

(defn nav-data [context selected]
  {:selected selected
   :items [{:name "games"
            :href (url context "games")
            :text "Games"}
           {:name "vms"
            :href (url context "vms")
            :text "VMs"}]})

(defn rest-chrome [{:keys [selected items] :as nav-data} body]
  (chrome
   [:ul.nav.nav-tabs
    (for [{:keys [name href text]} items]
      [:li {:class (if (= selected name) "active" "inactive")} [:a {:href href} text]])]
   [:div.tab-content
    body]))

(defn games-page [context {:keys [games] :as game-list}]
  (rest-chrome (nav-data context "games")
               [:div#games.row
                [:div.span4
                 [:ul.nav.nav-list
                  (for [game games]
                    [:li [:a {:href (get-in game [:_links :self :href]) } (:name game)]])]]]))

(defn game-page [context game]
  (rest-chrome (nav-data context "games")
               [:div#game
                [:div.span4
                 [:h2 (:name game)]
                 [:form {:method "post" :action (get-in game [:_links :launch :href])}
                  [:input {:type "hidden" :name "game" :value (:id game)}]
                  [:button.btn {:type "submit"} "Launch"]]]] ))

(defn vms-page [context vms]
  (rest-chrome (nav-data context "vms")
               [:div#vms.row
                [:dev.span4
                 [:ul.nav.nav-list
                  (for [{:keys [id _links]} vms]
                    [:li id
                     (for [[rel attrs] _links]
                       [:a (assoc attrs :rel rel) rel])])]]]))

(defn vm-page [context {:keys [id _links]}]
  (rest-chrome (nav-data context "vms")
               [:div#vms.row
                [:dev.span4
                 [:ul.nav.nav-list
                  [:span id]
                  (for [[rel attrs] _links]
                    [:a (assoc attrs :rel rel) rel])]]]))

(defn exec-page [context {:keys [id]}]
  (chrome
   [:script (str "vm_url = '" (url context "exec" id)  "';")]
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
  (info "Scanning for t3 resources.")
  (spy :info (->> (cp/classpath-directories)
                  (mapcat file-seq)
                  (map #(.getName %))
                  (filter #(.endsWith % ".t3")))))

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
  [system id]
  (peek (get @(:vm-histories system) id)))

(defn vm-host
  "Find the current host for the VM by id"
  [system id]
  (get @(:hosts system) id))

(defn vm-new!
  "Create a new VM for the game specified"
  [system game-id]
  (trace "vm-new!" game-id)
  (let [name (:name (game-get system game-id))
        prefix (first (string/split name #"\W"))
        histories (:vm-histories system)
        hosts (:hosts system)
        id (first (remove (partial contains? @histories) (repeatedly #(new-id prefix))))
        vm (assoc (t3vm/vm-from-image (parse/parse-resource name)) :id id)
        host ((:make-host system))]
    (info "Created VM: " id)
    (swap! histories assoc id [vm])
    (swap! hosts assoc id host)
    vm))

(defn vm-destroy!
  "Delete a VM by id"
  [system id]
  (info "Destroying VM: " id)
  (swap! (:vm-histories system) dissoc id)
  nil)

(defn vm-enter!
  "Enter the VM with specified id."
  [system id]
  (info "Entering VM: " id)
  (let [host (vm-host system id)
        [r s] ((t3vm/enter host) (vm-get system id))]
    (debugf "Updating store with %s/%d" id (:sequence s))
    (swap! (:vm-histories system) update-in [id] conj s)
    (vm-get system id)))

(defn vm-step!
  "Single-step the VM with specified id."
  [system id]
  (let [initial-state (vm-get system id)
        host (vm-host system id)]
    (debug "Stepping VM: " id " from sequence " (:sequence initial-state))
    (try
      (let [[e s] ((t3vm/step host pre-trace-step) initial-state)]
        (debugf "Updating store with %s/%d" id (:sequence s))
        (swap! (:vm-histories system) update-in [id] conj (assoc s :exc nil)))
      (catch Throwable t
        (error t "Error while stepping from " id "/" (:sequence initial-state))
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
  (let [initial-state (vm-get system id)
        host (vm-host system id)
        step (t3vm/step host pre-trace-step)
        error-handler (fn [s e]
                        (error e)
                        [e s])]
    (try
      (let [[e s] (t3vm/execute step initial-state error-handler)]
        (swap! (:vm-histories system) update-in [id] conj (assoc s :exc nil)))
      (catch Throwable t
        (swap! (:vm-histories system) update-in [id] conj (assoc initial-state :exc (with-out-str (st/print-stack-trace t))))))
    (vm-get system id)))

(defn dis1
  "Disassemble instruction at addr and return {:op {...} :args {...}}"
  [system id addr]
  (let [vm (vm-get system id)]
    (i/dis1 vm addr)))

(defn make-routes
  "Make routes for system"
  [context system]
  {:pre [(not (nil? system))
         (associative? system)]}
  (let [make-host (or (:make-host system) default-host)]
    (routes

     (GET "/games" []
       (respond context "t3chnique.server/games-page" (represent-game-list context (game-list system))))

     (GET ["/games/:id"] [id]
       (let [id (Integer/parseInt id)]
         (if-let [game (game-get system id)]
           (respond context "t3chnique.server/game-page" (represent-game context game))
           (response/not-found "Nonesuch"))))

     (GET "/vms" []
       (respond context "t3chnique.server/vms-page" (represent-vm-map context (vm-map system))))

     (GET ["/vms/:id"] [id]
       (if-let [vm (vm-get system id)]
         (respond context "t3chnique.server/vm-page" (represent-vm context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/vms/:id/stack"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-stack context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/vms/:id/registers"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-registers context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/vms/:id/code"] [id address length]
       (let [addr (Integer/parseInt address)
             len (Integer/parseInt length)]
         (if-let [vm (vm-get system id)]
           (respond context (represent-vm-code context id vm addr len))
           (response/not-found "Nonesuch"))))

     (GET ["/vms/:id/const"] [id address length]
       (let [addr (Integer/parseInt address)
             len (Integer/parseInt length)]
         (if-let [vm (vm-get system id)]
           (respond context (represent-vm-const context id vm addr len))
           (response/not-found "Nonesuch"))))

     (GET ["/vms/:id/objects"] [id oid count]
       (let [o (Integer/parseInt oid)
             n (Integer/parseInt count)]
         (if-let [vm (vm-get system id)]
           (respond context (represent-vm-objects context id vm o n))
           (response/not-found "Nonesuch"))))
     
     (GET ["/vms/:id/mcld"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-mcld context id vm))
         (response/not-found "Nonesuch")))

     (GET ["/vms/:id/fnsd"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-fnsd context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/vms/:id/symd"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-symd context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/vms/:id/exc"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-exc context id vm))
         (response/not-found "Nonesuch")))

     ;; actions
     
     (POST ["/vms"] [game]
       (let [game (Integer/parseInt game)]
         (response/redirect-after-post (url context "vms" (:id (vm-new! system game))))))

     (DELETE ["/vms/:id"] [id]
       (do
         (vm-destroy! system id)
         (response/redirect-after-post (url context "vms"))))
     
     (POST ["/vms/:id/step"] [id]
       (respond context (represent-vm context id (vm-step! system id))))
     
     (POST ["/vms/:id/enter"] [id]
       (respond context (represent-vm context id (vm-enter! system id))))
     
     (POST ["/vms/:id/run"] [id]
       (respond context (represent-vm context id (vm-run! system id))))

     (POST ["/vms/:id/back"] [id]
       (respond context (represent-vm context id (vm-back! system id))))
     
     (GET ["/vms/:id/dis1/:addr"] [id addr]
       (let [addr (Integer/parseInt addr)]
         (if-let [vm (vm-get system id)]
           (respond context (represent-vm-assembly context id vm addr (dis1 system id addr)))
           (response/not-found "Nonesuch"))))

     ;; register and stack state annotated with debug / tooling info
     (GET ["/vms/:id/inspect-state"] [id]
       (if-let [vm (vm-get system id)]
         (respond context (represent-vm-annotated-state context id vm))
         (response/not-found "Nonesuch")))
     
     (GET ["/exec/:id"] [id]
       (if-let [vm (vm-get system id)]
         (respond context "t3chnique.server/exec-page" (represent-vm context id vm))
         (response/not-found "Nonesuch")))
     
     (route/resources "/")
     
     (route/not-found "No such resource"))))

(defn make-app
  "Make a Ring app for serving the REST tools API. Context root must be
supplied for URL formation."
  [context system]
  (-> (handler/api (make-routes context system))
      (wrap-restful-params)
      (wrap-response)
      (wrap-stacktrace-web)))

(defn start-server
  "Start server with REST API at root context."
  [system]
  (j/run-jetty (make-app "/" system) {:port 8080 :join? false}))

(defn stop-server [system]
  (when-let [server @(:server system)]
    (.stop server)))

(defn system 
  "Initialise a new instance of the whole application."
  ([]
     (info "Initialising t3 system")
     (let [t3s (scan-for-t3)
           game-catalogue (map (fn [id name] {:id id :name name}) (range (count t3s)) t3s)]
       {:game-catalogue game-catalogue
        :vm-histories (atom {})
        :server (atom nil)
        :hosts (atom {})
        :make-host default-host})))

(defn start
  "Start an instance of the application."
  [system]
  {:pre [(not (nil? system))]}
  (info "Starting server")
  (swap! (:server system) (constantly (start-server system)))
  system)

(defn stop
  "Stop serving requests."
  [system]
  (info "Stopping server")
  (stop-server system)
  system)
