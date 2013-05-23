(ns t3chnique.metaclass.object
  (:require [t3chnique.metaclass :as mc])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]])
  (:import [t3chnique.metaclass MetaClass]))

(defn undef [])
(defn of-kind [])
(defn sclist [])
(defn propdef [])
(defn proptype [])
(defn get-prop-list [])
(defn get-prop-params [])
(defn is-class [])
(defn propinh [])
(defn is-transient [])

(def ftable [undef of-kind sclist propdef proptype get-prop-list get-prop-params is-class propinh is-transient])

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

  (get-property [self vm propid]
    ))

(defn tads-object
  ([] (TadsObject. nil nil nil))
  ([is-class bases properties] (TadsObject. is-class bases properties)))

(mc/register-metaclass! "tads-object/030005" tads-object)

