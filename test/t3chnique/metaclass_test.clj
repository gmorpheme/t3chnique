(ns t3chnique.test.metaclass_test
  (:use [clojure.test]
        [t3chnique.metaclass])
  (:require [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]))

(deftest tads-object-metaclass
  (let [m (vm/vm-state)
        o (tads-object true [] {1 (p/vm-funcptr 0x66) 2 (p/vm-true)})
        [oid m] ((vm/obj-store o) m)
        a (tads-object true [oid] {1 (p/vm-funcptr 0x88) 3 (p/vm-sstring "blah")})
        [aid m] ((vm/obj-store a) m)]
    
    ))