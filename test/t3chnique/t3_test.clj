(ns ^{:doc "Run compiled t3 test cases from TADS3 source"}
  t3chnique.t3-test
  (:require [t3chnique.monad :as m]
            [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.intrinsics.t3vm :as t3vm]
            [t3chnique.intrinsics.gen :as gen]
            [t3chnique.dump :refer [dump-state]]
            t3chnique.all
            [clojure.algo.monads :refer (update-val fetch-val m-seq m-chain)]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [trace debug info]])
  (:use [midje.sweet]))

(defrecord TraceHost [])

(defn to-string
  "A special to-string for say"
  [v]
  (cond
   (p/vm-string? v) (fn [s] [(vm/load-string-constant s (p/value v)) s])
   (p/vm-obj? v) (m/do-vm [obj (vm/obj-retrieve (p/value v))
                            text (mc/cast-to-string obj)]
                           text)
   (p/vm-int? v) (m/in-vm (m-result (str (p/value v))))
   (p/vm-list? v) (m/abort "TODO say to-string for lists")
   (p/vm-nil? v) (m/in-vm (m-result ""))
   :else (m/abort "VMERR_BAD_TYPE_BIF")))

(defmacro traced [f]
  `(fn [self# argc#]
    (trace ~(str f))
    (~f self# argc#)))

(extend TraceHost
    
  bif/t3vm
  {:t3RunGC (traced t3vm/t3RunGC)
   :t3GetVMVsn (traced t3vm/t3GetVMVsn)
   :t3GetVMID (traced t3vm/t3GetVMID)
   :t3GetVMBannder (traced t3vm/t3GetVMBanner)
   :t3GetVMPreinitMode (traced t3vm/t3GetVMPreinitMode)
   :t3DebugTrace (traced t3vm/t3DebugTrace)
   :t3SetSay (traced t3vm/t3SetSay)}
  
  bif/tads-gen
  {:dataType (traced gen/dataType)
   :firstObj (traced gen/firstObj)
   :nextObj (traced gen/nextObj)}

  bif/tads-io
  {:tadsSay (fn [_ argc]
              (trace "tads-io/tadsSay")
              (m/do-vm
               [args (m-seq (repeat argc (m-bind
                                          (vm/stack-pop)
                                          to-string)))
                _ (update-val :_output #(concat % args))
                _ (vm/reg-set :r0 (p/vm-nil))]
               nil))}

  bif/tads-net)

(defn trace-host []
  (TraceHost.))

(defn trace-step
  [host]
  (vm/step
   host
   (fn [op args ip]
     (trace "@" ip ":" (:mnemonic op))
     (m/do-vm
      [stack (fetch-val :stack)
       _ (update-val :_trace #(concat % [{:op op :args args :pre stack :ip ip}]))]
      nil))))

(defn format-stack
  ([stack]
     (format-stack stack nil))
  ([stack n]
     (let [stack-vals (if n (take-last n stack) stack)]
       (apply str (interpose " " (map p/mnemonise stack-vals))))))

(defn format-trace [{trc :_trace :as s}]
  (let [instructions (->> trc
                          (map (fn [{:keys [op args pre ip]}]
                                 (str "[..."
                                      (format-stack pre 5)
                                      "] ip:"
                                      ip
                                      " "
                                      (:mnemonic op)
                                      " "
                                      (apply str (interpose " " (map (fn [[k v]] (str (name k) ": " v)) args)))))))]
    (apply str (concat (interpose "\n" instructions)
                       ["\n\nstack: " (format-stack (:stack s))
                        "\n\noutput: " (apply str (:_output s))]))))


(defn trace-execution [name]
  (let [m0 (vm/vm-from-image (parse/parse-resource name))]
    (trace "===> Executing " name)
    (let [host (trace-host)
          m1 (m/exec-vm (vm/enter host) m0)
          m2 (assoc m1 :_trace [])
          stepper (trace-step host)]
      (vm/execute stepper m2
                  (fn error-handler [s e]
                    (spit (str "test/t3chnique/out/" name ".err")
                          (str (format-trace s) "\nexception: " e))
                    (dump-state s)
                    (throw e))))))

(defn run [name]
  (let [resource-name (str name ".t3")
        [r s] (trace-execution resource-name)
        trc (str (format-trace s)
                 (when r (str "\nreturn " (p/mnemonise r))))]
    (spit (str "test/t3chnique/out/" resource-name ".trc") trc)))

(defn compare-trace [name]
  (let [new (str "test/t3chnique/out/" name ".t3.trc")
        old (str "test/t3chnique/out/" name ".t3.good")]
    (and (.exists (io/file new))
         (= (string/trim (slurp new)) (string/trim (slurp old))))))

(fact "arith"
  (let [s (run "arith")]
    (compare-trace "arith") => true))

(fact "basic"
  (let [s (run "basic")]
    (compare-trace "basic") => true))

(fact "object"
  (let [s (run "object")]
    (compare-trace "object") => true))

(fact "dstr"
  (let [s (run "dstr")]
    (compare-trace "dstr") => true))

(fact "cube"
  (let [s (run "cube")]
    (compare-trace "cube") => true))
