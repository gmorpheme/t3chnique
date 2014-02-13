(ns ^{:doc "Tooling support. Functionality for rendering internal VM
structures for human inspection"}
  t3chnique.inspect
  (:require [t3chnique.monad :as m]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]))

(declare inspect-shallow)

(defn annotate-primitive
  "Annotate a primitive value with human-friendly information if possible."
  [vm val]
  (assoc val
    :mnemonic (p/mnemonise val)
    :detail (inspect-shallow vm val)))

(defmulti inspect-shallow
  "Inspect the value supplied to retrieve some useful debugging
information about its value or referent."
  (fn [vm val] (p/typeid val)))

(defmethod inspect-shallow p/vm-nil-id [vm val]
  ["nil"])

(defmethod inspect-shallow p/vm-true-id [vm val]
  ["true"])

(defmethod inspect-shallow p/vm-stack-id [vm val]
  (let [idx (p/value val)
        item (get-in vm [:stack idx])]
    [item (inspect-shallow vm item)]))

(defmethod inspect-shallow p/vm-obj-id [vm val]
  (let [oid (p/value val)
        {:keys [metaclass] :as  obj} (m/eval-vm (vm/obj-retrieve oid) vm)
        metaclass-name (:name (nth (:mcld vm) metaclass))]
    [metaclass-name]))

(defmethod inspect-shallow p/vm-prop-id [vm val]
  (let [symd (:symd vm)
        prop-name (first (filter #(= val (:value %)) symd))
        text (or prop-name (str "property[" (p/value val) "]"))]
    [text]))

(defmethod inspect-shallow p/vm-int-id [vm val]
  [(str (p/value val))])

(defmethod inspect-shallow p/vm-sstring-id [vm val]
  [(vm/load-string-constant vm (p/value val))])

(defmethod inspect-shallow p/vm-dstring-id [vm val]
  [(vm/load-string-constant vm (p/value val))])

(defmethod inspect-shallow p/vm-list-id [vm val]
  (mapcat inspect-shallow (vm/load-list-constant vm (p/value val))))

(defmethod inspect-shallow p/vm-bifptr-id [vm val]
  (let [[set idx] (p/bif-set-and-index val)
        set-name (nth (:fnsd vm) set)]
    [(str set-name ":" idx)]))

(defmethod inspect-shallow :default [vm val]
  ["(unknown)"])
