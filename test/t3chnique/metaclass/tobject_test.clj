(ns t3chnique.metaclass.tobject-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [monads.core :refer [mdo]]
            [monads.util :as u]
            [monads.state :refer [run-state exec-state eval-state]])
  (:use [t3chnique.metaclass.tobject])
  (:use [midje.sweet]))

(fact
  (let [buf (byte-array
             (map unchecked-byte '(0 0 8 0 1 0 1 0 11 -94 4 0 0 54 0 1 0 0 0 0 66 0 1 0 0 0 0 -90 0 1 0 0 0 0 -74 4 1 0 0 0 0 -66 4 10 -121 1 0 0 120 5 11 -52 4 0 0 82 6 1 0 0 0 0)))]
    (mc/load-from-image (tads-object) buf 0) => {:bases []
                                                 :is-class true
                                                 :properties {1 (p/vm-codeofs 1186)
                                                              54 (p/vm-nil)
                                                              66 (p/vm-nil)
                                                              166 (p/vm-nil)
                                                              1206 (p/vm-nil)
                                                              1214 (p/vm-list 391)
                                                              1400 (p/vm-codeofs 1228)
                                                              1618 (p/vm-nil)}}))

(facts "Deduping lists backwards"
  (dedupe-chain-backwards identity [:a :b :c :b :c :b]) => [:a :c :b]
  (dedupe-chain-backwards identity (apply concat [[:a :c] [:b :c] [:c]])) => [:a :b :c]
  (dedupe-chain-backwards identity (apply concat [[:c] [:b :c] [:a :c]])) => [:b :a :c])


(defn- obj [id bases props]
  (vm/obj-store id
                (-> (tads-object false bases props)
                    (assoc :oid id)
                    (assoc :metaclass 1))))

(facts "Test inheritance order with multiple inheritance"
  (let [vm (vm/vm-state)
        [[o1 o2 o3 o4] vm] (run-state (mdo
                                       (obj 1 [] {1 :a 2 :b 3 :c})
                                       (obj 2 [1] {2 :d 4 :e})
                                       (obj 3 [1] {3 :x 4 :y})
                                       (obj 4 [2 3] {1 :d 5 :z})
                                       (u/sequence-m (map vm/obj-retrieve [1 2 3 4]))) vm)]
    (:bases o1) => []
    (:bases o2) => [1]
    (:bases o3) => [1]
    (:bases o4) => [2 3]

    (map second (prop-chain vm o1 1)) => [:a]
    (map second (prop-chain vm o2 1)) => [:a]
    (map second (prop-chain vm o3 1)) => [:a]
    (map second (prop-chain vm o4 1)) => [:d :a]

    (map second (prop-chain vm o1 2)) => [:b]
    (map second (prop-chain vm o2 2)) => [:d :b]
    (map second (prop-chain vm o3 2)) => [:b]
    (map second (prop-chain vm o4 2)) => [:d :b]

    (map second (prop-chain vm o1 3)) => [:c]
    (map second (prop-chain vm o2 3)) => [:c]
    (map second (prop-chain vm o3 3)) => [:x :c]
    (map second (prop-chain vm o4 3)) => [:x :c]

    (map second (prop-chain vm o1 4)) => []
    (map second (prop-chain vm o2 4)) => [:e]
    (map second (prop-chain vm o3 4)) => [:y]
    (map second (prop-chain vm o4 4)) => [:e :y]

    (eval-state (mc/get-property o1 1 0) vm) => [(p/vm-obj 1) :a]
    (eval-state (mc/get-property o2 1 0) vm) => [(p/vm-obj 1) :a]
    (eval-state (mc/get-property o3 1 0) vm) => [(p/vm-obj 1) :a]
    (eval-state (mc/get-property o4 1 0) vm) => [(p/vm-obj 4) :d]

    (eval-state (mc/inherit-property o1 1 0) vm) => nil
    (eval-state (mc/inherit-property o2 1 0) vm) => [(p/vm-obj 1) :a]
    (eval-state (mc/inherit-property o3 1 0) vm) => [(p/vm-obj 1) :a]
    (eval-state (mc/inherit-property o4 1 0) vm) => [(p/vm-obj 1) :a]
    
    (eval-state (mc/get-property o4 4 0) vm) => [(p/vm-obj 2) :e]))

(let [vm (vm/vm-state)
      [[o1 o2] vm] (run-state (mdo
                               (obj 1 [] {1 (p/vm-int 1) 2 (p/vm-int 2)})
                               (obj 2 [1] {1 (p/vm-int 11)})
                               (u/sequence-m (map vm/obj-retrieve [1 2]))) vm)]
  (facts "VM property access helpers with tads-objects"
    (:r0 (exec-state (vm/generic-get-prop (p/vm-obj 2) 1 0) vm)) => (p/vm-int 11)
    (:r0 (exec-state (vm/generic-inherit-prop (p/vm-obj 2) 1 0) vm)) => (p/vm-int 1)))

(facts "Test is instance"
  (let [vm (vm/vm-state)
        [[o1 o2 o3 o4] vm] (run-state (mdo
                                       (obj 1 [] {1 :a 2 :b 3 :c})
                                       (obj 2 [1] {2 :d 4 :e})
                                       (obj 3 [1] {3 :x 4 :y})
                                       (obj 4 [2 3] {1 :d 5 :z})
                                       (u/sequence-m (map vm/obj-retrieve [1 2 3 4]))) vm)]
    (obj-chain vm o4) => [o4 o2 o3 o1]
    (eval-state (mc/is-instance? o4 (p/vm-obj 1)) vm) => true
    (eval-state (mc/is-instance? o4 (p/vm-obj 2)) vm) => true
    (eval-state (mc/is-instance? o4 (p/vm-obj 3)) vm) => true
    (eval-state (mc/is-instance? o4 (p/vm-obj 4)) vm) => true

    (eval-state (mc/is-instance? o4 (p/vm-obj 23)) vm) => false))

(facts "set-property"
  (let [vm (vm/vm-state)
        [object vm] (run-state (mdo
                                (obj 1 [] {4 (p/vm-int 1256)
                                           119 (p/vm-nil)
                                           14235 (p/vm-obj 2)})
                                (vm/obj-retrieve 1))
                               vm)]
    (eval-state (mc/set-property object 119 (p/vm-true)) vm) => (contains {:properties {4 (p/vm-int 1256)
                                                                                        119 (p/vm-true)
                                                                                        14235 (p/vm-obj 2)}})
    (eval-state (mc/set-property object 120 (p/vm-int 999)) vm) => (contains {:properties {4 (p/vm-int 1256)
                                                                                           119 (p/vm-nil)
                                                                                           120 (p/vm-int 999)
                                                                                           14235 (p/vm-obj 2)}})))
