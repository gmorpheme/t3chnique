(ns t3chnique.metaclass.grammar
  (:require [t3chnique.metaclass :as mc]
            [clojure.tools.logging :refer [trace]])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]))

(defrecord GrammarProduction []
  mc/MetaClass
  (mc/load-from-image [self buf o]
    ; TODO grammar load from image
    )
  )

(defn grammar
  ([]
     (trace "create grammar")
     (GrammarProduction.)))

(mc/register-metaclass! "grammar-production/030002" grammar)
(mc/register-data-reader! 't3chnique.metaclass.grammar.GrammarProduction
                          map->GrammarProduction)
