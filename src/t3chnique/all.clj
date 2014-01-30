(ns ^{:doc "Require VM together with implementations of all the standard metaclasses
and intrinstic function sets."}
  t3chnique.all
  (:require t3chnique.vm
            t3chnique.metaclass.object
            t3chnique.metaclass.tobject
            t3chnique.metaclass.string
            t3chnique.metaclass.vector
            t3chnique.metaclass.list
            [t3chnique.intrinsics :as bif]
            [t3chnique.intrinsics.t3vm :as t3vm]
            [t3chnique.intrinsics.gen :as gen]))

;; wire in BIF implementations to the default host

(defrecord Host [])

(extend Host
  
  bif/t3vm
  {:t3RunGC t3vm/t3RunGC
   :t3GetVMVsn t3vm/t3GetVMVsn
   :t3GetVMID t3vm/t3GetVMID
   :t3GetVMBannder t3vm/t3GetVMBanner
   :t3GetVMPreinitMode t3vm/t3GetVMPreinitMode
   :t3DebugTrace t3vm/t3DebugTrace
   :t3SetSay t3vm/t3SetSay}
  
  bif/tads-gen
  {:dataType gen/dataType
   :firstObj gen/firstObj
   :nextObj  gen/nextObj})

(defn default-host []
  (Host.))
