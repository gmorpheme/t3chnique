(ns t3chnique.metaclass.list
  (:require [t3chnique.metaclass :as mc])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(defrecord TadsList [val]
  mc/MetaClass
  (load-from-image [self buf o]
    (first ((domonad byteparser-m
              [n (uint2)
               values (times n (data-holder))]
              (TadsList. values)) [buf o]))))

(defn tads-list [] (TadsList. nil))

(mc/register-metaclass! "list/030008" tads-list)
