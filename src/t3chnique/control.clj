(ns ^{:doc "Common API for VM control (both web and cli)."}
  t3chnique.control
  (:require [t3chnique.parse :as parse]
            [t3chnique.vm :as t3vm]))

(def game-catalogue [{:id 1 :name "Elysium.t3"}
                     {:id 2 :name "ditch3.t3"}])

(defn game-list [] game-catalogue)

(defn game-get [id] (first (filter #(= (:id %) id) game-catalogue)))

(def vms (atom {}))

(defn vm-map [] @vms)

(defn vm-get [id] (get @vms (int id)))

(defn vm-new [game]
  (let [name  (:name (game-get game))
        vm (t3vm/vm-from-image (parse/parse-resource name))
        id (inc (count @vms)) ;; concurrency
        vm (assoc vm :id id)]
    (swap! vms assoc id vm)
    (vm-get id)))

(defn vm-step [id]
  (swap! vms update-in [id] #(second ((t3vm/step) %))))

; todo - patch in doc links http://www.tads.org/t3doc/doc/techman/t3spec/#opc_new1

(defn dis1
  "Disassemble instruction at addr and return {:op {...} :args {...}}"
  [id addr]
  (let [vm (vm-get id)]
    (-> (t3vm/offset vm addr)
        ((t3vm/parse-op))
        (first)
        ((fn [[op args]] {:op op :args args})))))