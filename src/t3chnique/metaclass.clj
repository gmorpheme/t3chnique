(ns t3chnique.metaclass
  (:require [clojure.string :as string]
            [t3chnique.intrinsics :as bif]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
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

(defonce metaclasses (atom {:dictionary2 unknown-metaclass
                            :grammar-production unknown-metaclass
                            :anon-func-ptr unknown-metaclass
                            :int-class-mod unknown-metaclass
                            :root-object unknown-metaclass
                            :intrinsic-class unknown-metaclass
                            :collection unknown-metaclass
                            :iterator unknown-metaclass
                            :indexed-iterator unknown-metaclass
                            :character-set unknown-metaclass
                            :bytearray unknown-metaclass
                            :regex-pattern unknown-metaclass
                            :lookuptable unknown-metaclass
                            :weakreflookuptable unknown-metaclass
                            :lookuptable-iterator unknown-metaclass
                            :file unknown-metaclass
                            :string-comparator unknown-metaclass
                            :bignumber unknown-metaclass
                            :stack-frame-desc unknown-metaclass}))

(defn register-metaclass! [metaclass-id constructor]
  (let [[id version] (bif/parse-id metaclass-id)
        kw (keyword id)]
    (swap! metaclasses assoc kw constructor)))

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)
          ctor (k @metaclasses)]
      (when (nil? ctor) (throw (RuntimeException. (str "Metaclass " k " not available"))))
      {:metaclass-id k :pids pids :metaclass ctor :_prototype (ctor)})))

(defn prototype
  "Return a metaclass prototype by state and index."
  [{:keys [mcld] :as state} index]
  (let [mcld-entry (nth mcld index)]
    (or (:_prototype mcld-entry) ((:metaclass mcld-entry)))))

(defn read-object-block
  "Read an image object block and represent as map of oid to obj
  map (as returned by metaclass load-from-image) together with
  metaclass and oid keys."
  [mcld oblock]
  (let [mcld-index (:mcld-index oblock)
        mclass-ctor (:metaclass (nth mcld mcld-index))
        prototype (mclass-ctor)]
    (into {} (map (fn [obj] [(:oid obj)
                            (-> (load-from-image prototype (:bytes obj) 0)
                                (assoc :metaclass mcld-index)
                                (assoc :oid (:oid obj)))])
                  (:objects oblock)))))

(defn get-intrinsic-method [{:keys [mcld] :as state} mcidx pid table]
  (let [{pids :pids} (nth mcld mcidx)
        dict (zipmap pids table)
        f (get dict pid nil)]
    (when f (p/vm-native-code f))))
