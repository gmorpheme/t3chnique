(ns t3chnique.metaclass.tobject
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.monad :as m])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]])
  (:import [t3chnique.metaclass MetaClass]))

(def tobj-table
  [
   (fn tobj-undef [argc] (m/abort "todo tobj-undef"))
   (fn tobj-create-instance [argc] (m/abort "todo tobj-create-instance"))
   (fn tobj-create-clone [argc] (m/abort "todo tobj-create-clone"))
   (fn tobj-create-trans-instance [argc] (m/abort "todo tobj-create-trans-instance"))
   (fn tobj-create-instance-of [argc] (m/abort "todo tobj-create-instance-of"))
   (fn tobj-create-trans-instance-of [argc] (m/abort "todo tobj-create-trans-instance-of"))
   (fn tobj-set-sc-list [argc] (m/abort "todo tobj-set-sc-list"))
   (fn tobj-get-method [argc] (m/abort "todo tobj-get-method"))
   (fn tobj-set-method [argc] (m/abort "todo tobj-set-method"))
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

(defn get-prop-intrinsic
  "Get a property by considering TadsObject's intrinsic methods."
  [state {mcidx :metaclass} pid]
  (mc/get-intrinsic-method state mcidx pid tobj-table))

(defn get-prop
  "Get property"
  [state self pid]
  (or (get-prop-from-chain state self pid)
      (get-prop-intrinsic state self pid)
      (obj/get-prop state self pid)))

(defrecord TadsObject [is-class bases properties]

  MetaClass

  (load-from-image [self buf o]
    (first
     ((domonad byteparser-m

        [base-count (uint2)
         prop-count (uint2)
         flags (uint2)
         bases (times base-count (uint4))
         properties (times prop-count (m-seq [(uint2) (data-holder)]))]

        (TadsObject.
         (= (bit-and flags 1) 1)
         bases
         (if (seq properties)
           (apply assoc {} (flatten properties))
           {}))) [buf o])))

  (get-property [self propid argc]
    (let [metaclass-index (:metaclass self)]
      (m/in-vm
       [[obj val] (m/m-apply #(get-prop % self propid))]

       ; eval intrinsic method if argc allows - otherwise return
       (if (and (p/vm-native-code? val) (not (nil? argc)))
         ((p/value val) argc)
         [(p/vm-obj (:oid obj)) val]))))

  (inherit-property [self propid argc]
    (m/in-vm
     [[obj val] (m/m-apply #(inh-prop-from-chain % self propid))]
     (when obj
       [(p/vm-obj (:oid obj)) val])))

  (list-like? [self state]
    )
  )

(defn tads-object
  ([] (TadsObject. nil nil nil))
  ([is-class bases properties] (TadsObject. is-class bases properties)))

(mc/register-metaclass! "tads-object/030005" tads-object)

