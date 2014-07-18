(ns t3chnique.metaclass.intcls
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [monads.core :refer [mdo return]]
            [clojure.tools.logging :refer [trace]])
  (:use [t3chnique.parse :only [parse-at skip uint2 uint4]]))

(defrecord IntrinsicClass [metaclass-index modifier-class]
  mc/MetaClass

  (mc/load-from-image [self buf o]
    (parse-at
     (mdo
      (skip 2)
      mc-id <- uint2
      modifier <- uint4
      (return (IntrinsicClass. mc-id modifier)))
     buf o))

  (mc/is-instance? [self val]
    (m/in-vm (m-result false))) ; TODO metaclass subclassing?
  
  )

(defn int-cls
  ([]
     (trace "create int-cls")
     (IntrinsicClass. nil nil)))

(mc/register-metaclass! "intrinsic-class/030001" int-cls)
