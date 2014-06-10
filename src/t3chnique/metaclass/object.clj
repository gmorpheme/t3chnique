(ns t3chnique.metaclass.object
  (:require [t3chnique.metaclass :as mc]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]]))


(def property-table
  [
   (fn undef [self argc])
   (fn of-kind [self argc])
   (fn sclist [self argc])
   (fn propdef [self argc])
   (fn proptype [self argc])
   (fn get-prop-list [self argc])
   (fn get-prop-params [self argc])
   (fn is-class [self argc])
   (fn propinh [self argc])
   (fn is-transient [self argc])
   ])

(defrecord RootObject []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    (RootObject.))
  )

(defn root
  ([]
     (trace "create root")
     (RootObject.)))

(mc/register-metaclass! "root-object/030004" root)
