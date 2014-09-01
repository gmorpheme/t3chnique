(ns t3chnique.metaclass.lookup
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]]))

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
(mc/register-data-reader! 't3chnique.metaclass.lookup.LookupTable map->LookupTable)
