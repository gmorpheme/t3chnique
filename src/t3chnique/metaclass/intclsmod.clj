(ns t3chnique.metaclass.intclsmod
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(defrecord IntrinsicClassModifier []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    ; TODO class mod load from image
    ))

(defn int-cls-mod
  ([] (IntrinsicClassModifier.)))

(mc/register-metaclass! "int-class-mod/030000" int-cls-mod)
