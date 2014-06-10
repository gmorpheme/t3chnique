(ns t3chnique.metaclass
  (:require [clojure.string :as string]
            [t3chnique.intrinsics :as bif]
            [t3chnique.primitive :as p]
            [t3chnique.monad :as m]
            [t3chnique.common :refer [indexed positions]]
            [clojure.tools.logging :refer [trace info spy]])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]]))

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
    "Monadic value to set property pid to value val. pid passed as number. Returns new obj.")
  )

(defrecord Unimplemented []
  MetaClass
  (load-from-image [self buf o]
    (Unimplemented.)))

(defn unknown-metaclass [] (Unimplemented.))

(defonce metaclasses (atom {
                            :collection unknown-metaclass
                            :iterator unknown-metaclass
                            :indexed-iterator unknown-metaclass
                            :character-set unknown-metaclass
                            :bytearray unknown-metaclass
                            :regex-pattern unknown-metaclass
                            :weakreflookuptable unknown-metaclass
                            :lookuptable-iterator unknown-metaclass
                            :file unknown-metaclass
                            :string-comparator unknown-metaclass
                            :bignumber unknown-metaclass
                            :stringbuffer unknown-metaclass}))

(defn register-metaclass! [metaclass-id constructor]
  (let [[id version] (bif/parse-id metaclass-id)
        kw (keyword id)]
    (info "Registering metaclass:" metaclass-id)
    (swap! metaclasses assoc kw constructor)))

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

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (trace "Metaclass table: " mcld)
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)
          ctor (k @metaclasses)]
      (info "Wiring up metaclass: " name)
      (when (nil? ctor)
        (throw (RuntimeException. (str "Metaclass " k " not available. Known metaclasses: " (keys @metaclasses)))))
      {:metaclass-id k :pids pids :metaclass ctor :_prototype (ctor)})))

(defn prototype
  "Return a metaclass prototype by state and index."
  [{:keys [mcld] :as state} index]
  (let [mcld-entry (nth mcld index)]
    (or (:_prototype mcld-entry) ((:metaclass mcld-entry)))))

(defn read-object-block
  "Read an image object block and represent as map of oid to obj
map (as returned by metaclass load-from-image) together with
metaclass and oid keys.
"
  [mcld oblock]
  (let [mcld-index (:mcld-index oblock)
        _ (trace "reading object block : " oblock)
        _ (trace "Metaclass table now: " mcld)
        mclass-ctor (:metaclass (nth mcld mcld-index))
        _ (trace "constructor : " mclass-ctor)
        prototype (mclass-ctor)]
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

Returns pair of the intrinsic class oid for the metaclass that defines
the intrinsic and the intrinsic method itself, which when called delivers
a monadic value.
"
  [state propid & mc-table-pairs]
  
  {:pre [(even? (count mc-table-pairs))
         (number? propid)]}
  (let [pairs (partition 2 mc-table-pairs)
        mcld (:mcld state)]
    (loop [[[name prop-table] & more] pairs]
      (when name
        (let [{pids :pids intcls :intrinsic-class-oid} (find-metaclass-by-id mcld name)]
          (if-let [property-index (first (positions #{propid} pids))]
            [(p/vm-obj intcls) (p/vm-native-code (get prop-table property-index))]
            (recur more)))))))

(defn lookup-intrinsic-m
  [propid & mc-table-pairs]
  (m/m-apply #(apply lookup-intrinsic % propid mc-table-pairs)))
