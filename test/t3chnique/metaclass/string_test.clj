(ns t3chnique.metaclass.string-test
  (:require [t3chnique.vm :as vm])
  (:require [t3chnique.metaclass.string :as mc-str]
            [monads.core :refer [mdo >>]]
            [monads.state :refer [exec-state]])
  (:use [midje.sweet]))

(fact
  (let [vm (exec-state (>>
                        (mc-str/create "left")
                        (mc-str/create "right")) (vm/vm-state))]
    (count (:objs vm)) => 2))

(fact
  (let [vm (exec-state (mdo
                        a <- (mc-str/create "a")
                        b <- (mc-str/create "b")
                        (mc-str/add-to-str a b)) (vm/vm-state))]
    (count (:objs vm)) => 3))


