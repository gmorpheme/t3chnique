(ns t3chnique.metaclass.iterator
  (:require [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.monad :as m]
            [clojure.tools.logging :refer [trace]]))

(defprotocol Iteration

  (iter-next [self] "Return [item new-iterator]")
  (has-next? [self])
  (reset [self])
  (current-key [self])
  (current-value [self]))

(def property-table
  [nil

   (fn getp-get-next [self argc]
     (vm/obj-intern (next self)))
   
   (fn getp-get-is-next-avail [self argc]
     (vm/obj-intern (has-next? self)))
   
   (fn getp-get-reset-iter [self argc] (m/abort "TODO getp-get-reset-iter"))
   (fn getp-get-cur-key [self argc] (m/abort "TODO getp-get-cur-key"))
   (fn getp-get-cur-val [self argc] (m/abort "TODO getp-get-cur-val"))])

(defrecord Iterator []
  mc/MetaClass

  ;; Search superclasses for inrinsic methods
  (get-property [self propid argc]
    (mc/default-get-property
      self propid argc
      :iterator property-table
      :root-object obj/property-table)))

(defn iterator []
  (trace "create iterator")
  (Iterator.))

(defrecord IndexIterator [coll idx]
  
  mc/MetaClass

  (get-property [self propid argc]
    (mc/default-get-property
      self propid argc
      :iterator property-table
      :root-object obj/property-table))

  Iteration

  (iter-next [self]
    [ (IndexIterator. coll (inc idx))])

  (has-next? [self]
    )
  )

(defn index-iterator []
  (IndexIterator. (p/vm-nil) 0))

(defn create-for-collection [coll]
  (IndexIterator. coll 0))

(mc/register-metaclass! "iterator/030001" iterator)
(mc/register-data-reader! 't3chnique.metaclass.iterator.Iterator map->Iterator)

(mc/register-metaclass! "indexed-iterator/030000" index-iterator)
(mc/register-data-reader! 't3chnique.metaclass.iterator.IndexIterator map->IndexIterator)
