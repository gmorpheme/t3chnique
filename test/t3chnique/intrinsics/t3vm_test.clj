(ns t3chnique.intrinsics.t3vm-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.algo.monads :refer [m-seq]])
  (:use midje.sweet
        t3chnique.util
        t3chnique.intrinsics.t3vm))

(fact "t3SetSay sets method"
  (:say-method (apply-ops
                (vm-state-with :stack [(p/vm-prop 0x10)])
                [(t3SetSay nil 1)]))
  => (p/vm-prop 0x10))

(fact "t3SetSay sets function"
  (:say-function (apply-ops
                  (vm-state-with :stack [(p/vm-funcptr 0x20)])
                  [(t3SetSay nil 1)]))
  => (p/vm-funcptr 0x20))

(fact "t3SetSay returns existing method"
  (:r0 (apply-ops
        (vm-state-with :say-method (p/vm-prop 0x30) :stack [(p/vm-prop 0x50)])
        [(t3SetSay nil 1)]))

  => (p/vm-prop 0x30))

(fact "t3SetSay returns existing function"
  (:r0 (apply-ops
        (vm-state-with :say-function (p/vm-funcptr 0x30) :stack [(p/vm-funcptr 0x50)])
        [(t3SetSay nil 1)]))

  => (p/vm-funcptr 0x30))

(fact "t3SetSay can blank method"
  (:say-method (apply-ops
                (vm-state-with :say-method (p/vm-prop 0x30) :stack [(p/vm-int 2)])
                [(t3SetSay nil 1)]))
  => (p/vm-prop 0))

(fact "t3SetSay can blank function"
  (:say-function (apply-ops
                  (vm-state-with :say-function (p/vm-funcptr 0x30) :stack [(p/vm-int 1)])
                  [(t3SetSay nil 1)]))
  => (p/vm-nil))

