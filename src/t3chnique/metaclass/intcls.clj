(ns t3chnique.metaclass.intcls
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [skip uint2 uint4 byteparser-m]]))

(defrecord IntrinsicClass [metaclass-index modifier-class]
  mc/MetaClass

  (mc/load-from-image [self buf o]
    (first
     ((domonad byteparser-m
        [_ (skip 2)
         meta-id (uint2)
         modifier (uint4)] ; TODO state
        (IntrinsicClass. meta-id modifier))
      [buf o])))

  (mc/is-instance? [self val]
    (m/in-vm (m-result false))) ; TODO metaclass subclassing?
  
  )

(defn int-cls
  ([]
     (trace "create int-cls")
     (IntrinsicClass. nil nil)))

(mc/register-metaclass! "intrinsic-class/030001" int-cls)
