(ns ^{:doc "Require VM together with implementations of all the standard metaclasses
and intrinstic function sets."}
  t3chnique.all
  (:require [t3chnique.vm :as vm]
            t3chnique.metaclass.object
            t3chnique.metaclass.tobject
            t3chnique.metaclass.string
            t3chnique.metaclass.vector
            t3chnique.metaclass.list))