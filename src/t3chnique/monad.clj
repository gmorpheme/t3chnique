(ns t3chnique.monad
  (:require [monads.state :as st]
            [monads.core :refer :all])
  (:require [clojure.algo.monads :as monads]))

(def vm-m monads/state-m)

(defmacro do-vm
  "Sugar: 'do'-syntax for running monadic values in the vm monad."
  [bindings val]
  `(monads/domonad vm-m ~bindings ~val))

(defmacro in-vm
  "Sugar: run mv in the vm monad."
  [mv]
  `(monads/with-monad vm-m ~mv))

(defn m->>
  "Sequence operations and discard the results."
  [& ops]
  (do-vm [_ (monads/m-seq ops)] nil))

(defn m-apply
  "Lift non monadic function with state as first arg to monadic
function."
  [f & args]
  {:pre [f]}
  (fn [s] {:pre [s]} [(apply f s args) s]))

(defn run-vm
  "Run monadic value against vm state and return value and state."
  [mv vm]
  (mv vm))

(defn eval-vm
  "Eval monadic value against vm state and return value"
  [mv vm]
  (first (mv vm)))

(defn exec-vm
  "Eval monadic value against vm state and return new state"
  [mv vm]
  (second (mv vm)))

(defn abort
  "For now, throw - incorporate into monad later."
  [msg]
  (throw (RuntimeException. ^String msg)))

