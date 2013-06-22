(ns t3chnique.intrinsics.gen
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass :as mc]
            [t3chnique.monad :as m]
            [clojure.algo.monads :refer [m-seq fetch-val fetch-state]]))

(defn- query-from-args 
  "Interpret args as firstObj/nextObj query"
  [argc]
  (m/do-vm
   [args (m-seq (repeat argc (vm/stack-pop)))
    :let [sc (or (first (filter p/vm-obj? args)) (p/vm-obj nil))
          flags (or (p/value (first (filter p/vm-int? args))) 1)
          include-classes (bit-and flags 2)
          include-instances (bit-and flags 1)]]
    [include-classes include-instances sc]))

(defn- object-matcher
  "Evaluate query match for an object in state s"
  [s start-id include-classes include-instances superclass]
  {:pre [(number? start-id)]}
  (fn [o]
    {:pre [#(not (nil? %))]}
    (and (>= (:oid o) start-id)
         (not false)             ; TODO intrinsic class mods
         (not false)             ; list or string classes also ignored
         (or
          (and (:is-class o) include-classes)
          (and (not (:is-class o)) include-instances))
         (or (not (p/valid? superclass))
             (m/eval-vm (mc/is-instance? o superclass) s)))))

(defn- enum-objects [argc start-id]
  (m/do-vm
   [[inc-cls inc-inst sc] (query-from-args argc)
    objs (fetch-val :objs)
    s (fetch-state)
    :let [matches (filter (object-matcher s start-id inc-cls inc-inst sc) (vals objs))]]
   (if-let [next-obj (first matches)]
     (p/vm-obj (:oid next-obj))
     (p/vm-nil))))

(defn dataType
  "Return the data type of the item at the top of the stack"
  [_ argc]
  (m/do-vm
   [val (vm/stack-pop)
    _ (vm/reg-set :r0 (p/vm-int (p/typeid val)))]
   nil))

(defn firstObj [_ argc]
  (m/do-vm
   [o (enum-objects argc 1)
    _ (vm/reg-set :r0 o)]
   nil))

(defn nextObj [_ argc]
  (m/do-vm
   [last-obj (vm/stack-pop)
    o (enum-objects (dec argc) (inc (p/value last-obj)))
    _ (vm/reg-set :r0 o)]
   nil))