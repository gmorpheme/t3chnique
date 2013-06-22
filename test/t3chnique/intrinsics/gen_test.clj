(ns t3chnique.intrinsics.gen-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.algo.monads :refer [m-seq]])
  (:use midje.sweet
        t3chnique.util
        t3chnique.intrinsics.gen))

(facts "Test object enumeration"
  (let [objs (:objs (m/exec-vm (m/in-vm
                                (m-seq [(obj 1 [] {})
                                        (obj 2 [1] {})
                                        (obj 3 [2] {})
                                        (obj 4 [1] {})
                                        (obj 5 [2 4] {})]))
                               (vm/vm-state)))
        with-stack (fn [stack] (assoc (vm-state-with :stack stack) :objs objs))]

    ;; enumerate all
    (let [vm (with-stack [])]
      (m/exec-vm (firstObj nil 0) vm) => (contains {:r0 (p/vm-obj 1)}))

    (let [vm (with-stack [(p/vm-obj 1)])]
      (m/exec-vm (nextObj nil 1) vm) => (contains {:r0 (p/vm-obj 2)}))
    
    (let [vm (with-stack [(p/vm-obj 2)])]
      (m/exec-vm (nextObj nil 1) vm) => (contains {:r0 (p/vm-obj 3)}))

    (let [vm (with-stack [(p/vm-obj 3)])]
      (m/exec-vm (nextObj nil 1) vm) => (contains {:r0 (p/vm-obj 4)}))

    ;; by superclass
    (let [vm (with-stack [(p/vm-obj 2)])]
      (m/exec-vm (firstObj nil 1) vm) => (contains {:r0 (p/vm-obj 2)}))
    
    (let [vm (with-stack [(p/vm-obj 2) (p/vm-obj 2)])]
      (m/exec-vm (nextObj nil 2) vm) => (contains {:r0 (p/vm-obj 3)}))

    (let [vm (with-stack [(p/vm-obj 2) (p/vm-obj 3)])]
      (m/exec-vm (nextObj nil 2) vm) => (contains {:r0 (p/vm-obj 5)}))))

(fact "dataType returns 1 for nil"
  (:r0 (apply-ops
        (vm-state-with :stack [(p/vm-nil)])
        [(dataType nil 1)]))
  => (p/vm-int 1))

(fact "dataType returns 7 for int"
  (:r0 (apply-ops
        (vm-state-with :stack [(p/vm-int 0)])
        [(dataType nil 1)]))
  => (p/vm-int 7))
