(ns t3chnique.metaclass.collection
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(def property-table [(fn getp_undef [self argc])
                     (fn getp_create_iter [self argc])
                     (fn getp_create_live_iter [self argc])])

(defrecord Collection [])

(defn collection
  ([] (Collection.)))

(mc/register-metaclass! "collection/030000" collection)
