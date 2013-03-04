(ns t3chnique.metaclass
  (:require [clojure.string :as string])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]]))

(defprotocol MetaClass
  "Operations available to the VM for each metaclass."
  (load-from-image [self buf o] "Load object data from byte buffer; return new object.")
  (get-property [self propid] "Return state function."))


(defrecord TadsObject [is-class bases properties]
  MetaClass
  (load-from-image [self buf o]
    ((domonad byteparser-m
      [base-count (uint2)
       prop-count (uint2)
       flags (uint2)
       :let [class (= (bit-and flags 1) 1)]
       bases (times base-count (uint4))
       properties (times prop-count (m-seq (uint2) (data-holder)))]
      (TadsObject. class bases (into {} properties))) [buf o])))

(defn tads-object
  ([] (TadsObject. nil nil nil))
  ([is-class bases properties] (TadsObject. is-class bases properties)))

(defrecord TadsString [text]
  MetaClass
  (load-from-image [self buf o]
    (with-monad byteparser-m
      (TadsString. ((prefixed-utf8) [buf o])))))

(defn tads-string [] (TadsString. nil))

(defrecord TadsList [val]
  MetaClass
  (load-from-image [self buf o]
    ((domonad byteparser-m
              [n (uint2)
               values (times n (data-holder))]
              (TadsList. values)) [buf o])))

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

(def metaclasses {:tads-object tads-object
                  :string tads-string
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
                  :bignumber unknown-metaclass})

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)
          metaclass (k metaclasses)]
      (when (nil? metaclass) (throw (RuntimeException. (str "Metaclass " k " not available"))))
      {:metaclass-id k :pids pids :metaclass metaclass})))

(defn read-object-block [mcld oblock]
  (let [mcld-index (:mcld-index oblock)
        mclass-ctor (:metaclass (nth mcld mcld-index))
        prototype (mclass-ctor)]
    (into {} (map (fn [obj] [(:oid obj)
                            (-> (load-from-image prototype (:bytes obj) 0)
                                (assoc :metaclass mcld-index))])
                  (:objects oblock)))))