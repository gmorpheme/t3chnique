(ns t3chnique.metaclass.object
  (:require [t3chnique.metaclass :as mc])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]])
  (:import [t3chnique.metaclass MetaClass]))


(def obj-table
  [
   (fn undef [])
   (fn of-kind [])
   (fn sclist [])
   (fn propdef [])
   (fn proptype [])
   (fn get-prop-list [])
   (fn get-prop-params [])
   (fn is-class [])
   (fn propinh [])
   (fn is-transient [])
   ])


(defn get-prop
  "TODO"
  [state self pid]
  nil)