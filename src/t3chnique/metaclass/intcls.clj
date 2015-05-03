(ns ^{:doc "Intrinsic class objects represent the intrinsic classes
 which are implemented by the virtual machine."}
  t3chnique.metaclass.intcls
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [monads.core :refer [mdo return]]
            [clojure.tools.logging :refer [trace]])
  (:use [t3chnique.parse :only [parse-at skip uint2 uint4]]))

(def intcls-table [])

;;; An intrinsic class object references the metaclass id and any
;;; modifier class that is used to augment the intrinsic class's
;;; default methods.
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
    (return (p/vm-nil)))                  ; TODO metaclass subclassing?
  
  (mc/get-property [self propid argc]
    (mc/default-get-property
      self propid argc
      :intrinsic-class intcls-table
      :root-object obj/property-table)))

(defn int-cls
  ([]
     (trace "create int-cls")
     (IntrinsicClass. nil nil)))

(mc/register-metaclass! "intrinsic-class/030001" int-cls)
(mc/register-data-reader! 't3chnique.metaclass.intcls.IntrinsicClass map->IntrinsicClass)
