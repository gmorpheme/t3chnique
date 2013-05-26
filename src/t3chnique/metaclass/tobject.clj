(ns t3chnique.metaclass.tobject
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as pr]
            [t3chnique.metaclass.object :as obj])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]]
        [t3chnique.monad :only [vm-m in-vm m-apply]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]])
  (:import [t3chnique.metaclass MetaClass]))

(def tobj-table
  [
   (fn getp-undef [])
   (fn getp-create-instance [])
   (fn getp-create-clone [])
   (fn getp-create-trans-instance [])
   (fn getp-create-instance-of [])
   (fn getp-create-trans-instance-of [])
   (fn getp-set-sc-list [])
   (fn getp-get-method [])
   (fn getp-set-method [])
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
  [state self pid]
  (first (prop-chain)))

(defn get-prop-intrinsic
  "Get a property by considering TadsObject's intrinsic methods."
  [{:keys [mcld] :as state} {mc-idx :metaclass :as self} pid]
  (let [{pids :pids} (nth mcld mc-idx)
        dict (zipmap pids tobj-table)
        f (get dict pid)]
    (pr/vm-native-code f)))

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

  (get-property [self propid]
    (let [metaclass-index (:metaclass self)]
      (in-vm
       [[obj val] (m-apply #(first (prop-chain % self propid)))

        mcld (fetch-val :mcld)
        :let [mc (nth mcld metaclass-index)
              lookup (zipmap (:pids mc) tobj-table)
              f (get lookup propid)]
        ]
       [obj val]))))

(defn tads-object
  ([] (TadsObject. nil nil nil))
  ([is-class bases properties] (TadsObject. is-class bases properties)))

(mc/register-metaclass! "tads-object/030005" tads-object)

