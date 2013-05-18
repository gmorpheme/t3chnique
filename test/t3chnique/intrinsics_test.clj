(ns t3chnique.intrinsics-test
  (:use [clojure.algo.monads]
        [midje.sweet])
  (:require [t3chnique.intrinsics :as bif]))

(defrecord TestHost []
  bif/t3vm
  (t3RunGC [_ argc] "t3RunGC")
  (t3SetSay [_ argc] "t3SetSay")
  (t3GetVMVsn [_ argc] "t3GetVMVsn")
  (t3GetVMID [_ argc] "t3GetVMID")
  (t3GetVMBanner [_ argc] "t3GetVMBanner")
  (t3GetVMPreinitMode [_ argc] "t3GetVMPreinitMode")
  (t3DebugTrace [_ argc] "t3DebugTrace")
  (t3GetGlobalSymbols [_ argc] "t3GetGlobalSymbols")
  (t3AllocProp [_ argc] "t3AllocProp")
  (t3GetStackTrace [_ argc] "t3GetStackTrace")
  (t3GetNamedArg [_ argc] "t3GetNamedArg")
  (t3GetNamedArgList [_ argc] "t3GetNamedArgList"))

(facts
  (let [host (TestHost.)]
    (bif/invoke-by-index host "t3vm/010005" 0 0) => "t3RunGC"
    (bif/invoke-by-index host "t3vm/010005" 1 0) => "t3SetSay"
    (bif/invoke-by-index host "t3vm/010005" 2 0) => "t3GetVMVsn"
    (bif/invoke-by-index host "t3vm/010005" 3 0) => "t3GetVMID"
    (bif/invoke-by-index host "t3vm/010005" 4 0) => "t3GetVMBanner"
    (bif/invoke-by-index host "t3vm/010005" 5 0) => "t3GetVMPreinitMode"
    (bif/invoke-by-index host "t3vm/010005" 6 0) => "t3DebugTrace"
    (bif/invoke-by-index host "t3vm/010005" 7 0) => "t3GetGlobalSymbols"
    (bif/invoke-by-index host "t3vm/010005" 8 0) => "t3AllocProp"
    (bif/invoke-by-index host "t3vm/010005" 9 0) => "t3GetStackTrace"
    (bif/invoke-by-index host "t3vm/010005" 10 0) => "t3GetNamedArg"
    (bif/invoke-by-index host "t3vm/010005" 11 0) => "t3GetNamedArgList"))