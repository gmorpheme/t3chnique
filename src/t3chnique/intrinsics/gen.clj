(ns ^{:doc "Implementations of tads-gen functions."}
  t3chnique.intrinsics.gen
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [monads.core :refer [mdo return >>= get-state]]
            [monads.util :as u]
            [monads.state :refer [eval-state]]
            [clojure.tools.logging :refer [info trace debug]]))

(defn dataType
  "Built-in. Return the data type of the item at the top of the stack"
  [_ argc]
  (>>= vm/stack-pop #(vm/reg-set :r0 (p/vm-int (p/typeid %)))))

(defn- object-query-opts-from-args 
  "Interpret stack args as options for firstObj / nextObj query. In
  the case of nextObj, the start-id will already have been popped from the stack."
  [argc]
  {:pre [(<= argc 2)]}
  
  (mdo
   args <- (u/sequence-m (repeat argc vm/stack-pop))
   (let [flags (or (first (filter p/vm-int? args)) 1)]
     (return {:include-classes (bit-and flags 2)
              :include-instances (bit-and flags 1)
              :superclass (or (first (filter p/vm-obj? args)) (p/vm-nil))}))))

(defn- object-matcher
  "Sequentially tests objects in s for match against the query options
specified."
  [s start-id {:keys [include-classes include-instances superclass]}]
  {:pre [(number? start-id)]}

  (fn [o]
    {:pre [#(not (or (nil? o) (p/vm-obj? o)))]} ;; full map not id
    (trace "Testing oid" o " for match")
    (and (>= (:oid o) start-id)
         (not false)             ; TODO intrinsic class mods
         (not false)             ; list or string classes also ignored
         (or
          (and (:is-class o) include-classes)
          (and (not (:is-class o)) include-instances))
         (or (not (p/valid? superclass))
             (eval-state (mc/is-instance? o superclass) s)))))

(defn- next-object
  "Implements firstObj and nextObj."
  [argc start-id]
  (mdo
   query-opts <- (object-query-opts-from-args argc)
   {objects :objs :as s}  <- get-state
   (let [matches (filter (object-matcher s start-id query-opts) (vals objects))]
     (return (if-let [next-obj (first matches)]
               (p/vm-obj (:oid next-obj))
               (p/vm-nil))))))

(defn firstObj
  "Built-in. Return the first object matching the query specified by
  the pushed args (superclass and flags)."
  [_ argc]
  (>>= (next-object argc 1) vm/vm-return))

(defn nextObj
  "Built-in. Return the next object matching the query specified by the pushed args.
First argument is object id to start from. Remainder are query
  opts (superclass and flags)."
  [_ argc]
  (mdo
   v <- vm/stack-pop
   o <- (next-object (dec argc) (inc (p/value v)))
   (vm/vm-return o)))
