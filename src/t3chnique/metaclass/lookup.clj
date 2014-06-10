(ns t3chnique.metaclass.lookup
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

(defrecord LookupTable []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    ; TODO lookup table load from image
    ))

(defn lookup-table
  ([]
     (trace "create lookup-table")
     (LookupTable.)))

(mc/register-metaclass! "lookuptable/030003" lookup-table)
