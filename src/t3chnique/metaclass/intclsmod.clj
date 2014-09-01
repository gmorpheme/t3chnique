(ns t3chnique.metaclass.intclsmod
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]]))

(defrecord IntrinsicClassModifier []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    ; TODO class mod load from image
    ))

(defn int-cls-mod
  ([]
     (trace "create int-cls-mod")
     (IntrinsicClassModifier.)))

(mc/register-metaclass! "int-class-mod/030000" int-cls-mod)
(mc/register-data-reader! 't3chnique.metaclass.intclsmod.IntrinsicClassModifier
                          map->IntrinsicClassModifier)
