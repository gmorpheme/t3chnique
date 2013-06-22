(ns t3chnique.util
  (:require [t3chnique.vm :refer [vm-state obj-store]]
            [t3chnique.metaclass.tobject :refer [tads-object]]
            [t3chnique.primitive :as p]
            [clojure.algo.monads :refer [with-monad state-m m-seq]]))

(defn st [& xs]
  (vec (map #(cond
              (number? %) (p/vm-int %)
              (string? %) (p/vm-sstring %)
              (nil? %) (p/vm-nil)
              (= % true) (p/vm-true)
              :else %)
            xs)))

(defn stack-after [& ops]
  (with-monad state-m
    (:stack (second ((m-seq ops) (vm-state))))))

(defn apply-ops [init ops]
  (with-monad state-m
    (second ((m-seq ops) init))))

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
