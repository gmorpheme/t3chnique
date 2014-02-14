(ns ^{:doc "Run compiled t3 test cases from TADS3 source"}
  t3chnique.t3-test
  (:require [t3chnique.monad :as m]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.intrinsics.t3vm :as t3vm]
            [t3chnique.intrinsics.gen :as gen]
            [clojure.algo.monads :refer (update-val)]
            [clojure.pprint :refer (pprint)])
  (:use [midje.sweet]))

(defrecord TraceHost [])
(extend TraceHost
    
  bif/t3vm
  {:t3RunGC t3vm/t3RunGC
   :t3GetVMVsn t3vm/t3GetVMVsn
   :t3GetVMID t3vm/t3GetVMID
   :t3GetVMBannder t3vm/t3GetVMBanner
   :t3GetVMPreinitMode t3vm/t3GetVMPreinitMode
   :t3DebugTrace t3vm/t3DebugTrace
   :t3SetSay t3vm/t3SetSay}
  
  bif/tads-gen
  {:dataType gen/dataType
   :firstObj gen/firstObj
   :nextObj  gen/nextObj}

  bif/tads-io
  bif/tads-net)

(defn trace-host []
  (TraceHost.))

(defn trace-step
  "Execute the op code referenced by the ip register."
  [host]
  (m/do-vm
   [[op args len] (vm/parse-op-at-ip)
    _ (update-val :ip (partial + len))
    _ (update-val :_trace #(conj % [op args]))
    ret (apply (:run-fn op) (cons host (vals args)))]
   ret))

(defn trace-execution [name]
  (let [m0 (vm/vm-from-image (parse/parse-resource name))]
    (let [host (trace-host)
          m1 (m/exec-vm (vm/enter host) m0)
          m2 (assoc m1 :_trace [])
          stepper (trace-step host)]

      (loop [s m1]
        (let [[r s+] (m/run-vm stepper s)]
          (if r
            [(:_trace s+) r]
            (recur s+)))))))

(defn format-trace [[trc ret]]
  (let [instructions (->> trc
                          (map (fn [[op args]]
                                 (str (:mnemonic op)
                                      " "
                                      (apply str (interpose " " (map (fn [[k v]] (str (name k) ": " v)) args)))))))]
    (apply str (concat (interpose "\n" instructions) ["\nreturn: " (p/mnemonise ret)]))))

(fact
  (format-trace (trace-execution "arith.t3")) =>
  "objgetprop obj_id: 8 prop_id: 39
builtin_a argc: 1 func_index: 1
pushfnptr code_offset: 373
return: nil")

