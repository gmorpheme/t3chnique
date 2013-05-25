(ns t3chnique.metaclass
  (:require [clojure.string :as string]
            [t3chnique.intrinsics :as bif])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]]))

(defprotocol MetaClass
  "Operations available to the VM for each metaclass."
  (load-from-image [self buf o]
    "Load object data from byte buffer; return new object.")
  
  (get-property [self propid]
    "Return [defining-object property-value]"))

(defrecord TadsString [text]
  MetaClass
  (load-from-image [self buf o]
    (with-monad byteparser-m
      (TadsString. (first ((prefixed-utf8) [buf o]))))))

(defn tads-string [] (TadsString. nil))

(defrecord TadsList [val]
  MetaClass
  (load-from-image [self buf o]
    (first ((domonad byteparser-m
              [n (uint2)
               values (times n (data-holder))]
              (TadsList. values)) [buf o]))))

(defn tads-list [] (TadsList. nil))

(defrecord AnonFn []
  MetaClass
  (load-from-image [self buf o]
    (AnonFn.)))

(defn anon-fn [] (AnonFn.))

(defrecord Unimplemented []
  MetaClass
  (load-from-image [self buf o]
    (Unimplemented.)))

(defn unknown-metaclass [] (Unimplemented.))

(defonce metaclasses (atom {:string tads-string
                            :list tads-list
                            :vector unknown-metaclass
                            :dictionary2 unknown-metaclass
                            :grammar-production unknown-metaclass
                            :anon-func-ptr anon-fn
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
                            :bignumber unknown-metaclass}))

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
          metaclass (k @metaclasses)]
      (when (nil? metaclass) (throw (RuntimeException. (str "Metaclass " k " not available"))))
      {:metaclass-id k :pids pids :metaclass metaclass})))

(defn read-object-block [mcld oblock]
  (let [mcld-index (:mcld-index oblock)
        mclass-ctor (:metaclass (nth mcld mcld-index))
        prototype (mclass-ctor)]
    (into {} (map (fn [obj] [(:oid obj)
                            (-> (load-from-image prototype (:bytes obj) 0)
                                (assoc :metaclass mcld-index)
                                (assoc :oid (:oid obj)))])
                  (:objects oblock)))))