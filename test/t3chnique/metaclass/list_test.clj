(ns t3chnique.metaclass.list-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.list :as lst]
            [monads.core :refer [>>]]
            [monads.state :refer [exec-state eval-state]])
  (:use [midje.sweet]))

(fact "Create dynamic lists"
  (let [vm (exec-state (>>
                        (lst/create [(p/vm-true) (p/vm-nil)])
                        (lst/create [(p/vm-int 8) (p/vm-codeofs 0x234)])) (vm/vm-state))]
    (count (:objs vm)) => 2
    
    (eval-state (vm/as-list (p/vm-obj 0)) vm) => [(p/vm-true) (p/vm-nil)]
    
    (eval-state (vm/as-list (p/vm-obj 1)) vm) => [(p/vm-int 8)
                                                  (p/vm-codeofs 0x234)]))
