(ns ^{:doc "The TADS Object metaclass. A prototypal object implementation 
where each object references base objects / classes and contains properties."}
  t3chnique.metaclass.tobject
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.object :as obj]
            [clojure.tools.logging :refer [trace spy debug]]
            [monads.core :refer [mdo return get-state]]
            [monads.util :refer [sequence-m]]
            [t3chnique.parse :refer [parse-at uint2 uint4 data-holder times prefixed-utf8 binary]]))

(defmacro abort [& args]
  `(throw (ex-info ~@args)))

(def tobj-table
  [
   (fn tobj-undef [argc] (abort "TODO tobj-undef"))
   (fn tobj-create-instance [argc] (abort "TODO tobj-create-instance"))
   (fn tobj-create-clone [argc] (abort "TODO tobj-create-clone"))
   (fn tobj-create-trans-instance [argc] (abort "TODO tobj-create-trans-instance"))
   (fn tobj-create-instance-of [argc] (abort "TODO tobj-create-instance-of"))
   (fn tobj-create-trans-instance-of [argc] (abort "TODO tobj-create-trans-instance-of"))
   (fn tobj-set-sc-list [argc] (abort "TODO tobj-set-sc-list"))
   (fn tobj-get-method [argc] (abort "TODO tobj-get-method"))
   (fn tobj-set-method [argc] (abort "TODO tobj-set-method"))
   ])

(defn dedupe-chain-backwards
  "Dedupe a sequence with respect to keyfn, ensuring only the
final instance remains in the sequence."
  [keyfn xs]
  (trace "deduping chain:" xs)
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
  (trace "prop-chain" pid)
  (let [inherited (->> bases
                       (map #(get (:objs state) %))
                       (mapcat #(prop-chain state % pid))
                       (dedupe-chain-backwards (comp :oid first)))]
    (trace "inherited:" inherited)
    (if (contains? properties pid)
      (cons [self (get properties pid)] inherited)
      inherited)))

(defn get-prop-from-chain
  "Get a property using the inheritance hierarchy."
  {:post [#(p/vm-primitive? (second %))]}
  [state self pid]
  (trace "get-prop-from-chain")
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

  ;; check for requested property in object otherwise look for intrinsics
  (mc/get-property [self propid argc]
    {:pre [(integer? propid) (integer? argc)]}
    (trace "tobject get-property" propid argc)
    (let [metaclass-index (:metaclass self)]
      (mdo
       st <- get-state
       (let [[obj val] (spy (get-prop st self propid))]
         ;; if it's an intrinsic and argc allows call, evaluate it
         (return (if (and (p/vm-native-code? val) (not (nil? argc)))
                   ((p/value val) argc)
                   [(p/vm-obj (:oid obj)) val])))))) ; TODO: could this be another type?

  ;; action to set property
  (mc/set-property [self propid val]
    {:pre [(integer? propid) (p/vm-primitive? val)]}
    (return (TadsObject. is-class bases (assoc properties propid val))))

  ;; check for requested property in inheritance chain - ignore
  ;; intrinsics
  (mc/inherit-property [self propid argc]
    {:pre [(integer? propid) (integer? argc)]}
    (mdo
     st <- get-state
     (let [[obj val] (inh-prop-from-chain st self propid)] ; TODO: intrinsics?
       (return (when obj
                 [(p/vm-obj (:oid obj)) val])))))

  ;; action for instance check
  (mc/is-instance? [self val]
    (mdo
     st <- get-state
     (return (not (empty? (filter
                           #(and (p/vm-obj? val) (= (p/value val) (:oid %)))
                           (obj-chain st self))))))))

(defn tads-object
  ([]
     (trace "create tads-object")
     (TadsObject. nil nil nil))
  ([is-class bases properties]
     (trace "create tads-object(" is-class bases properties ")")
     (TadsObject. is-class bases properties)))

(mc/register-metaclass! "tads-object/030005" tads-object)
(mc/register-data-reader! 't3chnique.metaclass.tobject.TadsObject map->TadsObject)
