(ns t3chnique.metaclass.dictionary2
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad]]))

(defrecord Dictionary2 []
  mc/MetaClass

  (load-from-image [self buf o]
    ;TODO dict load from image
    ))

(defn dictionary
  ([]
     (trace "create dictionary")
     (Dictionary2.)))

(mc/register-metaclass! "dictionary2/030001" dictionary)
(mc/register-data-reader! 't3chnique.metaclass.dictionary2.Dictionary2 map->Dictionary2)
