(ns t3chnique.intrinsics.t3vm
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.monad :as m]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]))

(defn t3RunGC [_ argc]
  (m/in-vm (m-result nil)))

(defn t3GetVMVsn [_ argc]
  (m/do-vm [_ (vm/reg-set :r0 (p/vm-int 0x00000001))] nil))

(defn t3GetVMID [_ argc]
  (m/do-vm [_ (vm/reg-set :r0 (p/vm-sstring "defn t3chnique"))] nil))

(defn t3GetVMBanner [_ argc]
  (m/do-vm [_ (vm/reg-set :r0 (p/vm-sstring "DEFN T3chnique Experimental TADS 3 VM - Copyright 2012 Greg Hawkins"))] nil))

(defn t3GetVMPreinitMode [_ argc]
  (m/do-vm [_ (vm/reg-set :r0 (p/vm-nil))] nil))

(defn t3DebugTrace [_ argc]
  (m/do-vm [_ (vm/reg-set :r0 (p/vm-nil))] nil))

(defn t3SetSay [_ argc]
  (let [in (fn [v] (if (p/vm-int? v)
                    (case (int (p/value v))
                      1 (p/vm-nil)
                      2 (p/vm-prop 0)
                      (m/abort "VMERR_BAD_TYPE_BIF"))
                    v))]
    (m/do-vm
     [val (vm/stack-pop)
      :let [val' (in val)]
      current (if (p/vm-prop? val') (vm/get-say-method) (vm/get-say-function))
      _ (if (p/vm-prop? val') (vm/set-say-method val') (vm/set-say-function val'))
      _ (vm/reg-set :r0 current)]
     nil)))