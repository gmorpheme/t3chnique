(ns t3chnique.elysium-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.parse :as parse]
            [t3chnique.metaclass :as mc]
            [t3chnique.monad :as m])
  (:use [midje.sweet]))

(let [vm (vm/vm-from-image (parse/parse-resource "Elysium.t3"))]

  (let [[o34 val] (first ((m/in-vm
                           [o34 (vm/obj-retrieve 34)
                            [do pv] (mc/get-property o34 183)]
                           [o34 pv]) vm))]
    (facts
      (:bases o34) => []
      val => {:type 2 :value nil})))