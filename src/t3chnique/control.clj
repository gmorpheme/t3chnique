(ns ^{:doc "Common API for VM control (both web and cli)."}
  t3chnique.control
  (:require [t3chnique.parse :as parse]
            [t3chnique.vm :as t3vm]
            [t3chnique.all]
            [clojure.stacktrace :as st]))

(def game-catalogue [{:id 1 :name "Elysium.t3"}
                     {:id 2 :name "ditch3.t3"}])

(defn game-list [] game-catalogue)

(defn game-get [id] (first (filter #(= (:id %) id) game-catalogue)))

(defonce vms (atom {}))

(defn vm-map [] @vms)

(defn vm-get [id] (get @vms (int id)))

(defn vm-new [game]
  (let [name  (:name (game-get game))
        vm (t3vm/vm-from-image (parse/parse-resource name))
        id (inc (count @vms)) ;; concurrency
        vm (assoc vm :id id)]
    (swap! vms assoc id vm)
    (vm-get id)))

(defn vm-new! [game]
  (vm-new game))

(defn vm-destroy! [id]
  (swap! vms dissoc id)
  nil)

(defn vm-put! [id game]
  (swap! vms assoc id game)
  id)

(defn vm-actions
  "Return the actions available for the vm at its current state"
  [vm]
  (if (zero? (:ip vm)) [:action/enter] [:action/step :action/run]))

(defn vm-enter [id]
  (let [[r s] ((t3vm/enter) (vm-get id))]
    (swap! vms assoc-in [id] s)
    r))

(defn vm-step [id]
  (try
    (let [[e s] ((t3vm/step) (vm-get id))]
      (swap! vms assoc-in [id] (assoc s :exc nil)))
    (catch Throwable t
      (swap! vms assoc-in [id :exc] (with-out-str (st/print-stack-trace t))))))

(defn vm-run! [id]
  (try
    (let [[e s] ((t3vm/run) (vm-get id))]
      (swap! vms assoc-in [id] (assoc s :exc nil)))
    (catch Throwable t
      (swap! vms assoc-in [id :exc] (with-out-str (st/print-stack-trace t))))))

(defn dis1
  "Disassemble instruction at addr and return {:op {...} :args {...}}"
  [id addr]
  (let [vm (vm-get id)]
    (-> (t3vm/offset vm addr)
        ((t3vm/parse-op))
        (first)
        ((fn [[op args]] {:op op :args args})))))