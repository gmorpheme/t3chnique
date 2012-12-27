(ns t3chnique.metaclass
  (:require [t3chnique.ber :as ber])
  (:require [clojure.string :as string]))

(defprotocol MetaClass
  "Operations available to the VM for each metaclass."
  (load-from-image [self buf o] "Load object data from byte buffer; return new object.")
  (get-property [self propid] "Return state function."))


(defrecord TadsObject [is-class bases properties]
  MetaClass
  (load-from-image [self buf o]
    (let [b (ber/slice buf)
          _ (.position b 0)
          base-count (ber/read-uint2 b)
          prop-count (ber/read-uint2 b)
          flags (ber/read-uint2 b)
          class (= (bit-and flags 1) 1)
          bases (loop [n 0 entries []]
                  (if (< n base-count)
                    (recur (inc n) (conj entries (ber/read-uint4 b)))
                    entries))
          properties (loop [n 0 entries {}]
                       (if (< n prop-count)
                         (let [pid (ber/read-uint2 b)
                               val (ber/read-data-holder b)]
                           (recur (inc n) (assoc entries pid val)))
                         entries))]
      (TadsObject. class bases properties)))
  (get-property [self pid]
    (if-let [prop (get (properties self) pid)]
      (fn [s] [prop s])
      #_(domonad state-m
               [scs (m-seq (map #(obj-retrieve %) bases))]
               (if-let [prop (some #(get (properties %) pid) scs)]
                 prop
                 (map (fn [sc] ((get-property sc pid))) scs))))))

(defn tads-object
  ([] (TadsObject. nil nil nil))
  ([is-class bases properties] (TadsObject. is-class bases properties)))

(defrecord TadsString [text]
  MetaClass
  (load-from-image [self buf o]
    (let [b (ber/slice buf)
          _ (.position b o)
          text (ber/read-pref-utf8 b)]
      (TadsString. text))))

(defn tads-string [] (TadsString. nil))

(defrecord TadsList [val]
  MetaClass
  (load-from-image [self buf o]
    (let [b (ber/slice buf)
          _ (.position b 0)
          count (ber/read-uint2 b)
          values (loop [n 0 entries []]
                   (if (< n count)
                     (recur (inc n) (conj entries (ber/read-data-holder b)))
                     entries))]
      (TadsList. values))))

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