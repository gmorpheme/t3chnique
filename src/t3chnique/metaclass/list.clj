(ns t3chnique.metaclass.list
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(defrecord TadsList [val]
  mc/MetaClass
  (mc/load-from-image [self buf o]
    (first ((domonad byteparser-m
              [n (uint2)
               values (times n (data-holder))]
              (TadsList. values)) [buf o])))

  (mc/get-as-seq [self]
    val))

(defn tads-list
  ([] (TadsList. nil))
  ([elems] (TadsList. elems)))

(defn create [elems]
  (m/do-vm
   [oid (vm/new-obj-id)
    _ (vm/obj-store oid (tads-list elems))]
   (p/vm-obj oid)))

(mc/register-metaclass! "list/030008" tads-list)
