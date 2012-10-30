(ns t3chnique.metaclass
  (:require [t3chnique.ber :as ber])
  (:require [clojure.string :as string]))

(defprotocol MetaClass
  (load-from-image [self buf o]))

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

(defrecord TadsString [text]
  MetaClass
  (load-from-image [self buf o]
    (let [b (ber/slice buf)
          _ (.position b o)
          text (ber/read-pref-utf8 b)]
      (TadsString. text))))

(defrecord DummyMetaclass [])

(def metaclasses {:tads-object TadsObject
                  :list DummyMetaclass
                  :dictionary2 DummyMetaclass
                  :grammar-production DummyMetaclass
                  :vector DummyMetaclass
                  :anon-func-ptr DummyMetaclass
                  :int-class-mod DummyMetaclass
                  :root-object DummyMetaclass
                  :intrinsic-class DummyMetaclass
                  :collection DummyMetaclass
                  :iterator DummyMetaclass
                  :indexed-iterator DummyMetaclass
                  :character-set DummyMetaclass
                  :bytearray DummyMetaclass
                  :string TadsString
                  :regex-pattern DummyMetaclass
                  :lookuptable DummyMetaclass
                  :weakreflookuptable DummyMetaclass
                  :lookuptable-iterator DummyMetaclass
                  :file DummyMetaclass
                  :string-comparator DummyMetaclass
                  :bignumber DummyMetaclass})

(defn wire-up-metaclasses
  "Takes MCLD block from image and wires in metaclass implementations"
  [mcld]
  (for [{:keys [name pids]} mcld]
    (let [[n version-required] (string/split name #"/")
          k (keyword n)
          metaclass (k metaclasses)]
      (when (nil? metaclass) (throw (RuntimeException. (str "Metaclass " k " not available"))))
      {:metaclass-id k :pids pids :metaclass metaclass})))