(ns ^{:doc "Implementations of tads-gen functions."}
  t3chnique.intrinsics.gen
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [t3chnique.monad :as m]
            [clojure.algo.monads :refer [m-seq fetch-val fetch-state]]))

(defn dataType
  "Built-in. Return the data type of the item at the top of the stack"
  [_ argc]
  (m/do-vm
   [val (vm/stack-pop)
    _ (vm/reg-set :r0 (p/vm-int (p/typeid val)))]
   nil))

(defn- object-query-opts-from-args 
  "Interpret stack args as options for firstObj / nextObj query. In
  the case of nextObj, the start-id will already have been popped from the stack."
  [argc]
  {:pre [(<= argc 2)]}
  
  (m/do-vm
   [args (m-seq (repeat argc (vm/stack-pop)))]

   (let [flags (or (first (filter p/vm-int? args)) 1)]

     {:include-classes (bit-and flags 2)
      :include-instances (bit-and flags 1)
      :superclass (or (first (filter p/vm-obj? args)) (p/vm-nil))})))

(defn- object-matcher
  "Sequentially tests objects in s for match against the query options
specified."
  [s start-id {:keys [include-classes include-instances superclass]}]
  {:pre [(number? start-id)]}

  (fn [o]
    {:pre [#(not (or (nil? o) (p/vm-obj? o)))]} ;; full map not id
    (and (>= (:oid o) start-id)
         (not false)             ; TODO intrinsic class mods
         (not false)             ; list or string classes also ignored
         (or
          (and (:is-class o) include-classes)
          (and (not (:is-class o)) include-instances))
         (or (not (p/valid? superclass))
             (m/eval-vm (mc/is-instance? o superclass) s)))))

(defn- next-object
  "Implements firstObj and nextObj."
  [argc start-id]
  (m/do-vm
   [query-opts (object-query-opts-from-args argc)
    objs (fetch-val :objs)
    s (fetch-state)]

   (let [matches (filter (object-matcher s start-id query-opts) (vals objs))]
     (if-let [next-obj (first matches)]
       (p/vm-obj (:oid next-obj))
       (p/vm-nil)))))

(defn firstObj
  "Built-in. Return the first object matching the query specified by
  the pushed args (superclass and flags)."
  [_ argc]
  (m/do-vm
   [o (next-object argc 1)
    _ (vm/reg-set :r0 o)]
   nil))

(defn nextObj
  "Built-in. Return the next object matching the query specified by the pushed args.
First argument is object id to start from. Remainder are query
  opts (superclass and flags)."
  [_ argc]
  (m/do-vm
   [last-obj (vm/stack-pop)
    o (next-object (dec argc) (inc (p/value last-obj)))
    _ (vm/reg-set :r0 o)]
   nil))
