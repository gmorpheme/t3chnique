(ns t3chnique.metaclass.root
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(defrecord RootObject [])

(defn root
  ([] (RootObject.)))

(mc/register-metaclass! "root-object/030004" root)
