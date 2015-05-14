(ns ^{:doc "Support WebSockets IO in games."}
  t3chnique.services.websockets
  (:require [org.httpkit.server :as http]
            [t3chnique.vm :as vm]
            [t3chnique.server :as sv]
            [t3chnique.intrinsics :as bif]
            [monads.core :as m :refer [mdo return >> >>=]]
            [monads.util :refer [sequence-m]]
            t3chnique.all
            [clojure.core.async :refer [go <! >! <!!]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [defroutes routes GET POST DELETE ANY context]]
            [compojure.handler :refer [site]]
            [compojure.route :refer [not-found resources]]
            [clojure.tools.logging :refer [info debug error spy]]))

(defn transform-thtml
  "Handle TADS3's weird HTML."
  [string]
  string ; TODO TADS TADS HTML translation
  )

(defn omkote-line-output
  "Transform lines into update maps for OmkOte."
  [lines]
  (map (fn [line]
         {:type :update
          :content [{:id 1 :clear false :text [{:append false :content ["normal" line]}]}]})
       lines))

(defn asyncTadsSay
  "Async implementation of tadsSay for the omkote web socketes implementation."
  [host argc]
  (let [out (bif/output-channel host)]
    (mdo
     args <- (sequence-m (repeat argc (vm/stack-pop)))
     let omkote-update = (->> args
                              (map (comp transform-thtml vm/convert-to-string))
                              (omkote-line-output))
     (go (>! out omkote-update))
     (return nil))))

(defn asyncInputLine
  "Async implementation of inputLine for the omkote web sockets implementation."
  [host argc]
  {:pre [(= 0 argc)]}
  (let [in (bif/input-channel host)]
    (mdo
     args <- (sequence-m (repeat argc (vm/stack-pop)))
     (return (<!! in)))))

(defrecord OmkoteWebSocketsHost [c->s s->c]

  bif/IAsyncHost
  (input-channel [_] c->s)
  (output-channel [_] s->c))

(extend OmkoteWebSocketsHost

  bif/tads-io
  {:tadsSay asyncTadsSay
   :inputLine asyncInputLine})

(defn make-ws-handler
  "Given host comms channels, create async handler for omkote
  websockets comms."
  [{:keys [c->s s->c] :as host}]
  (fn [req]
    (http/with-channel req channel
      (http/on-close channel (fn [status] (info "WS channel closed: " status)))
      (http/on-receive channel (fn [data]
                                 (info "WS received: " data)
                                 (http/send! channel (<!! (go (>! c->s data)
                                                              (<! s->c)))))))))
