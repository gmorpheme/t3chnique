(ns ^{:doc "Test game catalogue service"}
  t3chnique.services.game-catalogue-test
  (:require [midje.sweet :refer :all]
            [clojure.edn :as edn]
            [ring.mock.request :refer (request header)]
            [t3chnique.services.game-catalogue :refer :all]
            [com.stuartsierra.component :as component]
            [bidi.bidi :refer (match-route)]
            [bidi.ring :refer (make-handler)]
            [liberator.dev :refer (wrap-trace)]))

(fact "Scan finds games"
  (map :name
       (list-games
        (component/start (create-game-catalogue))))
  => (contains ["ditch3.t3" "Elysium.t3"] :gaps-ok :in-any-order))

(fact "Any game can be retrieved."
  (let [catalogue (component/start (create-game-catalogue))
        game-ids (map :id (list-games catalogue))]
    (doseq [id game-ids]
      (get-game catalogue id) => (contains {:name string?}))))

(fact "Image can be retrieved."
  (let [catalogue (component/start (create-game-catalogue))
        game-ids (map :id (list-games catalogue))
        data (get-image catalogue (rand-nth game-ids))]
    (take 2 data) => (seq "T3")))

(fact "Routes select correct resource"
  (match-route routes "/42") => {:handler :game-resource
                                   :route-params {:id "42"}}

  (match-route routes "/21/image") => {:handler :image-resource
                                        :route-params {:id "21"}})

;.;. The highest reward for a man's toil is not what he gets for it but
;.;. what he becomes by it. -- Ruskin
(fact "Get resource provides game metadata."
  (let [catalogue {:games (atom [{:id 21 :name "foo"}])}
        res (game-resource catalogue)]
    ((res {:params {:id "21"}})
     {:scheme :http
      :request-method :get
      :headers {"accept" "application/edn"}})
    => (contains {:status 200
                  :headers (contains {"Content-Type" #"application/edn.*"})
                  :body #(= (:name (edn/read-string %)) "foo")})))

(fact "Fetch image data"
  (let [res (image-resource {:games (atom [{:id 21 :name "Elysium.t3"}])})]
    ((res {:params {:id "21"}})
     {:scheme :http
      :request-method :get
      :headers {"accept" "application/x-t3vm-image"}})
    => (contains {:status 200
                  :headers (contains {"Content-Type" #".*t3vm.*"})
                  :body #(= (take 2 %) (seq "T3"))})))

(fact "Both metadata and image available through resource."
  (let [svc (make-handler routes (handlers {:games (atom [{:id 21 :name "ditch3.t3"}])}))]

    (let [req (-> (request :get "/21")
                  (header "accept" "application/edn"))]
      ((svc req) req)
      => (contains {:headers (contains {"Content-Type" #"application/edn.*"})}))

    (let [req (-> (request :get "/21/image")
                  (header "accept" "application/x-t3vm-image"))]
      ((svc req) req)
      => (contains {:headers (contains {"Content-Type" #"application/x-t3vm-image"})}))))
