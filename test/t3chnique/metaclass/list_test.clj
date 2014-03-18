(ns t3chnique.metaclass.list-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.list :as lst])
  (:use [midje.sweet]))

(fact "Create dynamic lists"
  (let [vm (m/exec-vm (m/m->>
                       (lst/create [(p/vm-true) (p/vm-nil)])
                       (lst/create [(p/vm-int 8) (p/vm-codeofs 0x234)])) (vm/vm-state))]
    (count (:objs vm)) => 2

    (m/eval-vm (m/do-vm [sq (vm/as-list (p/vm-obj 0))]
                        sq) vm) => [(p/vm-true) (p/vm-nil)]
    
    (m/eval-vm (m/do-vm [sq (vm/as-list (p/vm-obj 1))]
                        sq) vm) => [(p/vm-int 8) (p/vm-codeofs 0x234)]))
