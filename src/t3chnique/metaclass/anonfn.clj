(ns t3chnique.metaclass.anonfn
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]))

(defrecord AnonFunctionPtr []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    ;TODO anon fn load from image
    ))

(defn anon-fn
  ([]
     (trace "create anon-fn")
     (AnonFunctionPtr.)))

(mc/register-metaclass! "anon-func-ptr/030000" anon-fn)
(mc/register-data-reader! 't3chnique.metaclass.anonfn.AnonFunctionPtr map->AnonFunctionPtr)
