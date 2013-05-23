(ns t3chnique.metaclass.grammar
  (:require [t3chnique.metaclass :as mc])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]]))

