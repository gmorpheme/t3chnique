(ns t3chnique.metaclass
  (:require [t3chnique.ber :as ber])
  (:require [clojure.string :as string]))

(defprotocol MetaClass
  "Operations available to the VM for each metaclass."
  (load-from-image [self buf o] "Load object data from byte buffer; return new object."))

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
          properties (loop [n 0 entries []]
                       (if (< n prop-count)
                         (let [pid (ber/read-uint2 b)
                               val (ber/read-data-holder b)]
                           (recur (inc n) (conj entries {:pid pid :value val})))
                         entries))]
      (TadsObject. class bases properties))))

(defn tads-object [] (TadsObject. nil nil nil))

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

(defn unknown-metaclass [] nil)

(def metaclasses {:tads-object tads-object
                  :string tads-string
                  :list tads-list
                  :vector #(throw (RuntimeException. "Vector in image file."))
                  :dictionary2 unknown-metaclass
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
                  :bignumber unknown-metaclass})

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)
          metaclass (k metaclasses)
          _ (println (str "Metaclass " k " in image file."))]
      (when (nil? metaclass) (throw (RuntimeException. (str "Metaclass " k " not available"))))
      {:metaclass-id k :pids pids :metaclass metaclass})))

(defn read-object-block [mcld oblock]
  (let [mclass-ctor (:metaclass (nth mcld (:mcld-index oblock)))
        prototype (mclass-ctor)]
    (into {} (map (fn [obj] [(:oid obj) (load-from-image prototype (:bytes obj) 0)]) (:objects oblock)))))