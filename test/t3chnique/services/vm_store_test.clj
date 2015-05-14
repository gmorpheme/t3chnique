(ns ^{:doc "Test VM storage service"}
  t3chnique.services.vm-store-test
  (:require [midje.sweet :refer :all]
            [t3chnique.services.vm-store :refer :all]
            [com.stuartsierra.component :as component]))

(fact "Creates empty"
  (get-vm-ids (component/start (create-store))) => [])

(fact "Put a state sequence"
  (let [store (component/start (create-store))]
    (put-vm-state-seq store "test-id" [{} {} {}])
    (get-vm-ids store) => ["test-id"]))

(fact "Adopt a state"
  (let [store (component/start (create-store))]
    (adopt-vm-state store {})
    (count (get-vm-ids store)) => 1))

(fact "Adopt several states"
  (let [store (component/start (create-store))]
    (dotimes [i 10] (adopt-vm-state store {}))
    (count (get-vm-ids store)) => 10))

(fact "Testing backing up"
  (let [store (component/start (create-store))]
    (put-vm-state-seq store "blah" [{:id :a} {:id :b} {:id :c}])
    (back-up-vm store "blah")
    (get-latest-vm-state store "blah") => {:id :b}))
