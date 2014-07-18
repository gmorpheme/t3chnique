(ns t3chnique.metaclass.collection
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]))

(defn getp-undef [self argc]
  (trace "coll/getp-undef")
  (fn [s] [false s]))

(def property-table [getp-undef
                     (fn getp_create_iter [self argc])
                     (fn getp_create_live_iter [self argc])])

(defrecord Collection [])

(defn collection
  ([]
     (trace "create collection")
     (Collection.)))

(mc/register-metaclass! "collection/030000" collection)
