(ns t3chnique.metaclass.collection
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]]))

(defn getp-undef [self argc]
  (trace "coll/getp-undef")
  (fn [s] [false s]))

(defprotocol TadsIterable
  (create-iterator [coll])
  (create-live-iterator [coll]))

(def property-table
  [nil
   
   (fn getp_create_iter [self argc]
     (vm/obj-intern (create-iterator self)))
   
   (fn getp_create_live_iter [self argc]
     (vm/obj-intern (create-live-iterator self)))])

(defrecord Collection [])

(defn collection
  ([]
     (trace "create collection")
     (Collection.)))

(mc/register-metaclass! "collection/030000" collection)
(mc/register-data-reader! 't3chnique.metaclass.collection.Collection map->Collection)
