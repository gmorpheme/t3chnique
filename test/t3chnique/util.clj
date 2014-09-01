(ns t3chnique.util
  (:require [t3chnique.vm :refer [vm-state obj-store]]
            [t3chnique.metaclass.tobject :refer [tads-object]]
            [t3chnique.primitive :as p]
            [monads.state :refer [exec-state]]
            [monads.util :as u]))

(defmacro op
  "Automatically supply nil host argument."
  [operation & args]
  (let [op-name (symbol (str "op-" operation))]
    (list* op-name nil args)))

(defn st
  "Convenience function for creating a test stack"
  [& xs]
  (vec (map #(cond
              (number? %) (p/vm-int %)
              (string? %) (p/vm-sstring %)
              (nil? %) (p/vm-nil)
              (= % true) (p/vm-true)
              :else %)
            xs)))

(defn stack-after [& ops]
  (:stack (exec-state (u/sequence-m ops) (vm-state))))

(defn apply-ops [init ops]
  (exec-state (u/sequence-m ops) init))

(defn apply-with-stack [stack ops]
  (:stack (apply-ops (merge (vm-state) {:stack stack :sp (count stack)}) ops)))

(defn vm-state-with [& args]
  (let [vm (apply assoc (vm-state) args)]
    (assoc vm :sp (count (:stack vm)))))

(defn obj [id bases props]
  (obj-store id
             (-> (tads-object false bases props)
                 (assoc :oid id)
                 (assoc :metaclass 1))))
