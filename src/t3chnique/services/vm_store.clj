(ns ^{:doc "VM storage microservice"}
  t3chnique.services.vm-store
  (:require [com.stuartsierra.component :as component]
            [liberator.core :refer (resource)]
            [t3chnique.services.webmount :as webmount]))

(defn new-id
  "Create a new id string with prefix"
  [prefix]
  (apply str prefix \- (take 6 (->> (repeatedly (partial rand-int 26))
                                    (map (partial + 97))
                                    (map char)))))

(defn new-unique-id
  "Create a new id that is not already a key in histories"
  [prefix histories]
  (first (remove (partial contains? histories) (repeatedly #(new-id prefix)))))

(defn get-vm-ids
  "Return the ids of all the VMs in the store."
  [{:keys [vm-histories]}]
  (or (keys @vm-histories) []))

(defn get-latest-vm-states
  "Return map of id to most recent VM state."
  [{:keys [vm-histories]}]
  (map (fn [[k v]] [k (peek v)] @vm-histories)))

(defn get-latest-vm-state
  "Return latest VM state for specified id."
  [{:keys [vm-histories]} id]
  (peek (get @vm-histories id)))

(defn get-host-for-vm
  "Return the host in use for specified VM."
  [{:keys [hosts]} id]
  (get @hosts id))

(defn put-vm-state-seq
  "Put specified state sequence as history under id using a default host."
  [{:keys [vm-histories hosts]} id state-sequence]
  {:pre [(coll? state-sequence)]}
  (swap! vm-histories assoc id state-sequence))

(defn adopt-vm-state
  "Adopt a VM at a given state assigning a new ID."
  [store state]
  (put-vm-state-seq store (new-unique-id "import-" @(:vm-histories store)) [state]))

(defn delete-vm
  "Delete the entire history of a VM."
  [{:keys [vm-histories]} id]
  (swap! vm-histories dissoc id)
  nil)

(defn back-up-vm
  "Pop the most recent item off the named history."
  [{:keys [vm-histories]} id]
  (swap! vm-histories update-in [id] pop))

;;;;;;;;;;;;;;
;; REST API ;;
;;;;;;;;;;;;;;

(defn vm-resource
  [vm-store]
  (fn [{:keys [params]}]
    (resource
     :allowed-methods [:get]
     :available-media-types ["application/json" "application/edn"]
     :handle-ok? (get-latest-vm-state vm-store (:id params)))))

(def routes
  ["/" {[:id] :vm-resource}])

(defn handlers [vm-store]
  {:vm-resource (vm-resource vm-store)})

(defrecord Store [vm-histories hosts]
  component/Lifecycle
  (start [self] self)
  (stop [self] self)
  webmount/WebMount
  (routes [self] routes)
  (handlers [self] (handlers self)))

(defn create-store
  "Create an empty VM store"
  []
  (Store. (atom {}) (atom {})))
