(ns t3chnique.elysium-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.parse :as parse]
            [t3chnique.metaclass :as mc]
            [t3chnique.all]
            [monads.core :refer [mdo return]]
            [monads.state :refer [eval-state]])
  (:use [midje.sweet]))

(let [vm (vm/vm-from-image (parse/parse-resource "Elysium.t3"))]

  (let [query (mdo
               o34 <- (vm/obj-retrieve 34)
               [do pv] <- (mc/get-property o34 183 0)
               (return pv))]
    (fact "Object thirty four checks"
      (eval-state query vm) => {:type 2 :value nil})))
