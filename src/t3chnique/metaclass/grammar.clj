(ns t3chnique.metaclass.grammar
  (:require [t3chnique.metaclass :as mc])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]]))

(defrecord GrammarProduction []
  mc/MetaClass
  (mc/load-from-image [self buf o]
    ; TODO grammar load from image
    )
  )

(defn grammar
  ([] (GrammarProduction.)))

(mc/register-metaclass! "grammar-production/030002" grammar)
