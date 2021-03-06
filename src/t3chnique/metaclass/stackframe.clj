(ns t3chnique.metaclass.stackframe
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace]]))

(defrecord StackFrameDesc []
  mc/MetaClass

  (mc/load-from-image [self buf o]

    ))

(defn stack-frame-desc
  ([] (StackFrameDesc.)))

(defrecord StackFrameRef []
  mc/MetaClass

  (mc/load-from-image [self buf o]
    ))

(defn stack-frame-ref
  ([]
     (trace "create stack-frame-ref")
     (StackFrameRef.)))

(mc/register-metaclass! "stack-frame-desc/030000" stack-frame-desc)
(mc/register-data-reader! 't3chnique.metaclass.stackframe.StackFrameDesc map->StackFrameDesc)

(mc/register-metaclass! "stack-frame-ref/030000" stack-frame-ref)
(mc/register-data-reader! 't3chnique.metaclass.stackframe.StackFrameRef map->StackFrameRef)
