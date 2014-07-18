(ns ^{:doc "The TADS Object metaclass. A prototypal object implementation 
where each object references base objects / classes and contains properties."}
  t3chnique.metaclass.tobject
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.monad :as m]
            [clojure.tools.logging :refer [trace]]
            [monads.core :refer [mdo return]]
            [monads.util :refer [sequence-m]])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val fetch-state]]
        [t3chnique.parse :only [parse-at uint2 uint4 data-holder times prefixed-utf8 binary]]))

(def tobj-table
  [
   (fn tobj-undef [argc] (m/abort "TODO tobj-undef"))
   (fn tobj-create-instance [argc] (m/abort "TODO tobj-create-instance"))
   (fn tobj-create-clone [argc] (m/abort "TODO tobj-create-clone"))
   (fn tobj-create-trans-instance [argc] (m/abort "TODO tobj-create-trans-instance"))
   (fn tobj-create-instance-of [argc] (m/abort "TODO tobj-create-instance-of"))
   (fn tobj-create-trans-instance-of [argc] (m/abort "TODO tobj-create-trans-instance-of"))
   (fn tobj-set-sc-list [argc] (m/abort "TODO tobj-set-sc-list"))
   (fn tobj-get-method [argc] (m/abort "TODO tobj-get-method"))
   (fn tobj-set-method [argc] (m/abort "TODO tobj-set-method"))
   ])

(defn dedupe-chain-backwards
  "Dedupe a sequence with respect to keyfn, ensuring only the
final instance remains in the sequence."
  [keyfn xs]
  (loop [seen #{}
         rem (reverse xs)
         ret []]
    (let [[one & rest] rem
          k (keyfn one)]
      (if (seq rem)
        (if (seen k)
          (recur seen rest ret)
          (recur (conj seen k) rest (cons one ret)))
        ret))))

(defn obj-chain
  [state {:keys [bases properties] :as self}]
  (let [inherited (->> bases
                       (map #(get (:objs state) %))
                       (mapcat #(obj-chain state %))
                       (dedupe-chain-backwards :oid))]
    (cons self inherited)))

(defn prop-chain
  "Return seq of [defining val] based on inheritance hierarchy."
  [state {:keys [bases properties] :as self} pid]
  (let [inherited (->> bases
                       (map #(get (:objs state) %))
                       (mapcat #(prop-chain state % pid))
                       (dedupe-chain-backwards (comp :oid first)))]
    (if (contains? properties pid)
      (cons [self (get properties pid)] inherited)
      inherited)))

(defn get-prop-from-chain
  "Get a property using the inheritance hierarchy."
  {:post [#(p/vm-primitive? (second %))]}
  [state self pid]
  (first (prop-chain state self pid)))

(defn inh-prop-from-chain
  [state self pid]
  (let [pchain (prop-chain state self pid)]
    (if (= (ffirst pchain) self)
      (second pchain)
      (first pchain))))

(defn get-prop
  "Get property"
  [state self pid]
  (or (get-prop-from-chain state self pid)
      (mc/lookup-intrinsic state pid
                       :tads-object tobj-table
                       :root-object obj/property-table)))

(defrecord TadsObject [is-class bases properties]

  mc/MetaClass

  (mc/load-from-image [self buf o]
    (parse-at
     (mdo
      base-count <- uint2
      prop-count <- uint2
      flags <- uint2
      bases <- (times base-count uint4)
      properties <- (times prop-count (sequence-m [uint2 data-holder]))
      (return (TadsObject. (= (bit-and flags 1) 1)
                           bases
                           (if (seq properties)
                             (apply assoc {} (flatten properties))
                             {})))) buf o))

  (mc/get-property [self propid argc]
    {:pre [(number? propid)]}
    (let [metaclass-index (:metaclass self)]
      (m/do-vm
       [[obj val] (m/m-apply #(get-prop % self propid))]

                                        ; eval intrinsic method if argc allows - otherwise return
       (if (and (p/vm-native-code? val) (not (nil? argc)))
         ((p/value val) argc)
         [(p/vm-obj (:oid obj)) val]))))

  (mc/set-property [self propid val]
    {:pre [(number? propid) (p/vm-primitive? val)]}
    (m/do-vm
     []
     (TadsObject. is-class bases (assoc properties propid val))))

  (mc/inherit-property [self propid argc]
    {:pre [(number? propid)]}
    (m/do-vm
     [[obj val] (m/m-apply #(inh-prop-from-chain % self propid))]
     (when obj
       [(p/vm-obj (:oid obj)) val])))

  (mc/list-like? [self state]
                                        ;    TODO
    )

  (mc/is-instance? [self val]
    (m/do-vm
     [s (fetch-state)]
     (not (empty? (filter
                   #(and (p/vm-obj? val)
                         (= (p/value val) (:oid %)))
                   (obj-chain s self))))))

  (mc/get-as-string [self]))

(defn tads-object
  ([]
     (trace "create tads-object")
     (TadsObject. nil nil nil))
  ([is-class bases properties]
     (trace "create tads-object(" is-class bases properties ")")
     (TadsObject. is-class bases properties)))

(mc/register-metaclass! "tads-object/030005" tads-object)

