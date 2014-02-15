(ns t3chnique.metaclass.string-test
  (:require [t3chnique.vm :as vm])
  (:require [t3chnique.monad :as m])
  (:require [t3chnique.metaclass.string :as mc-str])
  (:use [midje.sweet]))

(fact
  (let [vm (m/exec-vm (m/m->>
                       (mc-str/create "left")
                       (mc-str/create "right")) (vm/vm-state))]
    (count (:objs vm)) => 2))


(fact
  (let [vm (m/exec-vm (m/do-vm
                       [a (mc-str/create "a")
                        b (mc-str/create "b")
                        c (mc-str/add-to-str a b)]
                       nil) (vm/vm-state))]
    (count (:objs vm)) => 3))


