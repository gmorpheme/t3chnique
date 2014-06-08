(ns t3chnique.metaclass.list
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.collection :as coll]
            [t3chnique.metaclass.object :as obj]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(def property-table
  [(fn getp_undef [self argc] (m/abort "TODO "))
   (fn getp_subset [self argc] (m/abort "TODO "))
   (fn getp_map [self argc] (m/abort "TODO "))
   (fn getp_len [self argc] (m/abort "TODO "))
   (fn getp_sublist [self argc] (m/abort "TODO "))
   (fn getp_intersect [self argc] (m/abort "TODO "))
   (fn getp_index_of [self argc] (m/abort "TODO "))
   (fn getp_car [self argc] (m/abort "TODO "))
   (fn getp_cdr [self argc] (m/abort "TODO "))
   (fn getp_index_which [self argc] (m/abort "TODO "))
   (fn getp_for_each [self argc] (m/abort "TODO "))
   (fn getp_val_which [self argc] (m/abort "TODO "))
   (fn getp_last_index_of [self argc] (m/abort "TODO "))
   (fn getp_last_index_which [self argc] (m/abort "TODO "))
   (fn getp_last_val_which [self argc] (m/abort "TODO "))
   (fn getp_count_of [self argc] (m/abort "TODO "))
   (fn getp_count_which [self argc] (m/abort "TODO "))
   (fn getp_get_unique [self argc] (m/abort "TODO "))
   (fn getp_append_unique [self argc] (m/abort "TODO "))
   (fn getp_append [self argc] (m/abort "TODO "))
   (fn getp_sort [self argc] (m/abort "TODO "))
   (fn getp_prepend [self argc] (m/abort "TODO "))
   (fn getp_insert_at [self argc] (m/abort "TODO "))
   (fn getp_remove_element_at [self argc] (m/abort "TODO "))
   (fn getp_remove_range [self argc] (m/abort "TODO "))
   (fn getp_for_each_assoc [self argc] (m/abort "TODO "))
   (fn getp_generate [self argc] (m/abort "TODO "))
   (fn getp_splice [self argc] (m/abort "TODO "))
   (fn getp_join [self argc] (m/abort "TODO "))
   (fn getp_indexOfMin [self argc] (m/abort "TODO "))
   (fn getp_minVal [self argc] (m/abort "TODO "))
   (fn getp_indexOfMax [self argc] (m/abort "TODO "))
   (fn getp_maxVal [self argc] (m/abort "TODO "))])

(defrecord TadsList [val]
  mc/MetaClass

  ; Byte format is simple prefixed list of data holders
  (mc/load-from-image [self buf o]
    (first ((domonad byteparser-m
              [n (uint2)
               values (times n (data-holder))]
              (TadsList. values)) [buf o])))

  ; Get property - intrinsics and superclass properties only
  (mc/get-property [self propid argc]
    {:pre [(number? propid)]}
    (m/do-vm
     [method (mc/lookup-intrinsic-m propid
                                    :list property-table
                                    :collection coll/property-table
                                    :object obj/property-table)
      r ((p/value method) self argc)]
     r))
  
  (mc/get-as-seq [self]
    val))

(defn tads-list
  "Create non-interned list (no :oid or :metaclass)."
  ([] (TadsList. nil))
  ([elems] (TadsList. elems)))

(defn create [elems]
  (m/do-vm
   [oid (vm/new-obj-id)
    _ (vm/obj-store oid (tads-list elems))]
   (p/vm-obj oid)))

(mc/register-metaclass! "list/030008" tads-list)
