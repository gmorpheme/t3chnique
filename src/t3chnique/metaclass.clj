(ns t3chnique.metaclass
  (:require [clojure.string :as string]
            [t3chnique.intrinsics :as bif]
            [t3chnique.primitive :as p]
            [t3chnique.common :refer [indexed positions]]
            [clojure.tools.logging :refer [trace info spy]]
            [monads.core :refer [mdo return get-state]])
  (:use [t3chnique.parse :only [uint2 uint4 data-holder times record prefixed-utf8]]))

;; Protocols for APIs which need to be exposed to the VM
;; implementation itself.
;;
;; MetaClass is basic facilities required by all objects. In addition,
;; Iteration is provided (as required by iternext op code).

(defprotocol MetaClass
  "Operations available to the VM for each metaclass."

  (load-from-image [self buf o]
    "Load object data from byte buffer; return new object.")

  (load-from-stack [_ argc]
    "Return monadic value to create from stack.")

  (get-property [self propid argc]
    "Monadic value to return [defining-object property-value]. When argc
is not nil, intrinsic methods may be invoked. Otherwise a vm-native-code
value may be returned to indicate that an intrinsic method would have
been invoked. propid passed as number.")

  (inherit-property [self propid argc]
    "Monadic value to return [defining-object property-value. When argc
is not nil, intrinsic methods may be invoked. Otherwise a vm-native-code
value may be returned to indicate that an intrinsic method would have
been invoked. propid passed as number.")

  (list-like? [self vm]
    "Whether the object is list like")

  (is-instance? [self val]
    "Monadic value to determine whether self is a subclass of val.")

  (get-as-string [self]
    "Expose internal String if appropriate or nil otherwise. (Non-monadic)")

  (get-as-seq [self]
    "Expose internal list as seq if appropriate or nil otherwise. (Non-monadic)")

  (cast-to-string [self]
    "Cast to String if possible or nil otherwise. (Monadic)")

  (add [self val]
    "Monadic value to add val to self and return the sum. Returns nil if not possible
and alternative strategies should be attempted (op overloading).")

  (set-property [self pid val]
    "Monadic value to set property pid to value val. pid passed as number. Returns new obj."))

(defprotocol Iteration

  (iter-next [self] "Return [item new-iterator]")
  (has-next? [self])
  (reset [self])
  (current-key [self])
  (current-value [self]))

(defrecord Unimplemented []
  MetaClass
  (load-from-image [self buf o]
    (Unimplemented.)))

(defn unknown-metaclass [] (Unimplemented.))

(defonce metaclasses (atom {:character-set unknown-metaclass
                            :bytearray unknown-metaclass
                            :regex-pattern unknown-metaclass
                            :weakreflookuptable unknown-metaclass
                            :lookuptable-iterator unknown-metaclass
                            :file unknown-metaclass
                            :string-comparator unknown-metaclass
                            :bignumber unknown-metaclass
                            :stringbuffer unknown-metaclass}))

(defonce data-readers (atom {}))

(defn register-metaclass! [metaclass-id constructor]
  (let [[id version] (bif/parse-id metaclass-id)
        kw (keyword id)]
    (info "Registering metaclass:" metaclass-id)
    (swap! metaclasses assoc kw constructor)))

(defn register-data-reader!
  "Registers a data-reader for reading the record from dump files."
  [symbol map-constructor]
  (swap! data-readers #(assoc % symbol map-constructor)))

(register-data-reader! 't3chnique.metaclass.Unimplemented map->Unimplemented)

(defn- find-metaclass-index-by-id
  "Find metaclass in mcld by full id match."
  [mcld id]
  (first
   (keep-indexed
    #(when (= id (:metaclass-id %2)) %1)
    mcld)))

(defn find-metaclass-by-id
  "Find metaclass in cld by name / keyword match."
  [mcld metaclass-id]
  (first
   (filter
    #(= metaclass-id (:metaclass-id %))
    mcld)))

(defn hydrate-mcld [s]
  (assoc s :mcld
         (for [{:keys [metaclass-id] :as metaclass} (:mcld s)]
           (let [ctor (metaclass-id @metaclasses)
                 prototype (ctor)]
             (assoc metaclass :_prototype prototype)))))

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (trace "Metaclass table: " mcld)
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)]
      (info "Wiring up metaclass: " name)
      (if-let [ctor (k @metaclasses)]
        (let [prototype (ctor)]
          (info "Prototype is: " prototype)
          {:metaclass-id k :pids pids :_prototype prototype})
        (throw (RuntimeException. (str "Metaclass " k " not available. Known metaclasses: " (keys @metaclasses))))))))

(defn prototype
  "Return a metaclass prototype by state and index."
  [{:keys [mcld] :as state} index]
  (let [mcld-entry (nth mcld index)]
    (:_prototype mcld-entry)))

(defn read-object-block
  "Read an image object block and represent as map of oid to obj
map (as returned by metaclass load-from-image) together with
metaclass and oid keys.
"
  [mcld oblock]
  (let [mcld-index (:mcld-index oblock)
        _ (trace "reading object block : " oblock)
        _ (trace "Metaclass table now: " mcld)
        prototype (:_prototype (nth mcld mcld-index))]
    (into {} (map (fn [obj] [(:oid obj)
                            (-> (load-from-image prototype (:bytes obj) 0)
                                (assoc :metaclass mcld-index)
                                (assoc :oid (:oid obj)))])
                  (:objects oblock)))))

(defn process-intrinsic-class-objects
  "Process all the objects and link back any metaclasses to the 
intrinsic class objects that represent them by storing oid in metaclass
under key :intrinsic-class-oid. Return enhanced mcld."
  [mcld objs]
  {pre [(vector? mcld) (vector? objs)]}
  (info "Wiring up intrinsic class objects")
  (into []
        (reduce
         (fn [mcs [oid obj]]
           (trace "Considering object" obj)
           (if-let [idx (:metaclass-index obj)]
             (do
               (info "Wired in intrinsic class object " oid " for metaclass " idx)
               (update-in mcs [idx :intrinsic-class-oid] (constantly oid)))
             mcs))
         mcld
         objs)))

(defn lookup-intrinsic
  "Lookup an intrinsic method by property id against a sequence of
   metaclass id / method table pairs. e.g.

   (lookup-intrinsic vm 23 :list list/property-table :collection coll/property-table ..)

   Returns pair of the intrinsic class oid for the metaclass that
   defines the intrinsic and the intrinsic method itself as
   vm-native-code, which when called delivers a monadic value."
  [state propid & mc-table-pairs]
  
  {:pre [(even? (count mc-table-pairs))
         (number? propid)]}
  (let [pairs (partition 2 mc-table-pairs)
        mcld (:mcld state)]
    (loop [[[name prop-table] & more] pairs]
      (when name
        (let [{pids :pids intcls :intrinsic-class-oid} (find-metaclass-by-id mcld name)]
          (trace "PIDS: " pids " IntCls: " intcls)
          (if-let [property-index (first (positions #{propid} pids))] ; zero is undef prop
            (let [f (get prop-table (inc property-index))]
              (trace "Intrinsic Method: " f)
              [(p/vm-obj intcls) (p/vm-native-code f)])
            (recur more)))))))

(defn default-get-property
  "Action to lookup intrinsic method and invoke it (returning [intcls action])."
  [self propid argc & mc-table-pairs]
  {:pre [(number? propid)]}
  (trace "default-get-property" propid argc)
  (mdo
   st <- get-state
   (let [[intcls method] (apply lookup-intrinsic st propid mc-table-pairs)]
     (return [intcls ((p/value method) self argc)]))))
