(ns t3chnique.metaclass.stackframe
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m]
            [t3chnique.primitive :as p])
  (:use [clojure.algo.monads :only [domonad]]
        [t3chnique.parse :only [uint2 byteparser-m data-holder times]]))

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
  ([] (StackFrameRef.)))

(mc/register-metaclass! "stack-frame-desc/030000" stack-frame-desc)
(mc/register-metaclass! "stack-frame-ref/030000" stack-frame-ref)
