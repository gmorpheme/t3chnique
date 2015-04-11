(ns ^{:doc "Run compiled t3 test cases from TADS3 source"}
  t3chnique.t3-test
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.intrinsics.t3vm :as t3vm]
            [t3chnique.intrinsics.gen :as gen]
            [t3chnique.dump :refer [dump-state]]
            t3chnique.all
            [clojure.pprint :refer (pprint)]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [monads.core :refer [mdo >>= return]]
            [monads.util :as u]
            [monads.state :refer [exec-state]]
            [clojure.tools.logging :refer [trace tracef debug info error]])
  (:use [midje.sweet]))

(defn convert-to-string-for-say
  "Return internal string representation (not vm-sstring)."
  [v]
  {:pre [(p/vm-primitive? v)]}
  (trace "convert-to-string" v)
  (cond

   (or (p/vm-sstring? v)
       (p/vm-dstring? v)) (vm/get-val #(vm/load-string-constant % (p/value v)))
   
   (p/vm-obj? v) (mdo
                  obj <- (vm/obj-retrieve (p/value v))
                  (return (mc/get-as-string obj))) ;TODO metaclass cast_to_string

   (p/vm-int? v) (return (str (p/value v)))

   (p/vm-nil? v) (return "")
   
   (p/vm-true? v) (return "true")
   
   :else (throw (ex-info "TODO other string conversions" {:value (p/mnemonise v)}))))


(defrecord TraceHost [])

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
              (mdo
               args <- (u/sequence-m (repeat argc (>>=
                                                   vm/stack-pop
                                                   convert-to-string-for-say)))
               (vm/update-val :_output #(concat % args))
               (vm/vm-return (p/vm-nil))))}

  bif/tads-net)

(defn trace-host []
  (TraceHost.))

(defn trace-step
  [host]
  (vm/step
   host
   (fn [op args ip pc]
     (tracef "@%d..%d: %s" ip pc (:mnemonic op))
     (mdo
      stack <- (vm/get-val :stack)
      (vm/update-val :_trace #(concat % [{:op op :args args :pre stack :ip ip}]))))))

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
    (info "===> Executing " name)
    (let [host (trace-host)
          m1 (exec-state (vm/enter host) m0)
          m2 (assoc m1 :_trace [])
          stepper (trace-step host)]
      (info "Executing now")
      (vm/execute stepper m2
                  (fn error-handler [s e]
                    (error e)
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

     ;.;. Actual: false
   ;.;. Expected: true
;.;. FAIL "object" at (form-init2319925236458664615.clj:4)
(fact "object"
  (let [s (run "object")]
    (compare-trace "object") => true))

(fact "dstr"
  (let [s (run "dstr")]
    (compare-trace "dstr") => true))

(future-fact "cube"
  (let [s (run "cube")]
    (compare-trace "cube") => true))
