(ns t3chnique.metaclass.list
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.iterator :as iter]
            [t3chnique.metaclass.collection :as coll]
            [t3chnique.metaclass.object :as obj]
            [monads.core :refer [>>= mdo]]
            [clojure.tools.logging :refer [trace]])
  (:use [t3chnique.parse :only [parse uint2 data-holder times]]))

(defmacro abort [& args]
  `(throw (ex-info ~@args)))

(def property-table
  [(fn getp_undef [self argc] (abort "TODO getp_undef"))
   (fn getp_subset [self argc] (abort "TODO getp_subset"))
   (fn getp_map [self argc] (abort "TODO getp_map"))
   (fn getp_len [self argc] (abort "TODO getp_len"))
   (fn getp_sublist [self argc] (abort "TODO getp_sublist"))
   (fn getp_intersect [self argc] (abort "TODO getp_intersect"))
   (fn getp_index_of [self argc] (abort "TODO getp_index_of"))
   (fn getp_car [self argc] (abort "TODO getp_car"))
   (fn getp_cdr [self argc] (abort "TODO getp_cdr"))
   (fn getp_index_which [self argc] (abort "TODO getp_index_which"))
   (fn getp_for_each [self argc] (abort "TODO getp_for_each"))
   (fn getp_val_which [self argc] (abort "TODO getp_val_which"))
   (fn getp_last_index_of [self argc] (abort "TODO getp_last_index_of"))
   (fn getp_last_index_which [self argc] (abort "TODO getp_last_index_which"))
   (fn getp_last_val_which [self argc] (abort "TODO getp_last_val_which"))
   (fn getp_count_of [self argc] (abort "TODO getp_count_of"))
   (fn getp_count_which [self argc] (abort "TODO getp_count_which"))
   (fn getp_get_unique [self argc] (abort "TODO getp_get_unique"))
   (fn getp_append_unique [self argc] (abort "TODO getp_append_unique"))
   (fn getp_append [self argc] (abort "TODO getp_append"))
   (fn getp_sort [self argc] (abort "TODO getp_sort"))
   (fn getp_prepend [self argc] (abort "TODO getp_prepend"))
   (fn getp_insert_at [self argc] (abort "TODO getp_insert_at"))
   (fn getp_remove_element_at [self argc] (abort "TODO getp_remove_element_at"))
   (fn getp_remove_range [self argc] (abort "TODO getp_remove_range"))
   (fn getp_for_each_assoc [self argc] (abort "TODO getp_for_each_assoc"))
   (fn getp_generate [self argc] (abort "TODO getp_generate"))
   (fn getp_splice [self argc] (abort "TODO getp_splice"))
   (fn getp_join [self argc] (abort "TODO getp_join"))
   (fn getp_indexOfMin [self argc] (abort "TODO getp_indexOfMin"))
   (fn getp_minVal [self argc] (abort "TODO getp_minVal"))
   (fn getp_indexOfMax [self argc] (abort "TODO getp_indexOfMax"))
   (fn getp_maxVal [self argc] (abort "TODO getp_maxVal"))])

(defrecord TadsList [val]
  mc/MetaClass

  ;; Byte format is simple prefixed list of data holders
  (mc/load-from-image [self buf o]
    (parse
     (>>= uint2 #(times % data-holder) #(TadsList. %))
     [buf o]))

  ;; Search superclasses for intrinsics methods
  (mc/get-property [self propid argc]
    (mc/default-get-property
      self propid argc
      :list property-table
      :collection coll/property-table
      :root-object obj/property-table))
  
  (mc/get-as-seq [self]
    val)

  ;; Provide index iterators
  coll/TadsIterable

  (create-iterator [self]
    (iter/create-for-collection self)))

(defn tads-list
  "Create non-interned list (no :oid or :metaclass)."
  ([]
     (trace "creat tads-list")
     (TadsList. nil))
  ([elems]
     (trace "create tads-list" elems)
     (TadsList. elems)))

(defn create
  "Action: create list from elems and intern."
  [elems]
  (vm/obj-intern (tads-list elems)))

(mc/register-metaclass! "list/030008" tads-list)
(mc/register-data-reader! 't3chnique.metaclass.list.TadsList map->TadsList)
