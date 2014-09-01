(ns t3chnique.intrinsics.t3vm
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [monads.core :refer [return mdo >>=]]))

(defn t3RunGC
  "Action: run garbage collector. Ignored."
  [_ argc]
  (return nil))

(defn t3GetVMVsn
  "Intrinsic Function: return VM version number in r0."
  [_ argc]
  (vm/vm-return (p/vm-int 0x00000001)))

(defn t3GetVMID
  "Intrinsic Function: return VM name (t3chnique)"
  [_ argc]
  (throw (ex-info "TODO: implement VM ID")))

(defn t3GetVMBanner
  "Intrinsic Function: return VM banner"
  [_ argc]
  (throw (ex-info "TODO: implement VM banner")))

(defn t3GetVMPreinitMode
  "Intrinsic Function: return whether in pre-init mode (false)"
  [_ argc]
  (vm/vm-return (p/vm-nil)))

(defn t3DebugTrace
  "Intrinsic Function: return whether in debug trace mode (false)"
  [_ argc]
  (vm/vm-return (p/vm-nil)))

(defn t3SetSay
  "Intrinsic Function: set say method or function"
  [_ argc]
  (let [in (fn [v] (if (p/vm-int? v)
                    (case (int (p/value v))
                      1 (p/vm-nil)
                      2 (p/vm-prop 0)
                      (throw (ex-info "VMERR_BAD_TYPE_BIF" {:code :VMERR_BAD_TYPE_BIF})))
                    v))]
    (mdo
     arg <- vm/stack-pop
     let new-value = (in arg), method? = (p/vm-prop? new-value)
     current <- (if method? vm/get-say-method vm/get-say-function)
     (if method? (vm/set-say-method new-value) (vm/set-say-function new-value))
     (vm/vm-return current))))
