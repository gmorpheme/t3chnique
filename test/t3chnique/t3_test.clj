(ns ^{:doc "Run compiled t3 test cases from TADS3 source"}
  t3chnique.t3-test
  (:require [t3chnique.monad :as m]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.intrinsics.t3vm :as t3vm]
            [t3chnique.intrinsics.gen :as gen]
            [clojure.algo.monads :refer (update-val fetch-val m-seq)]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as string])
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
  {:tadsSay (fn [_ argc]
              (m/do-vm
               [args (m-seq (repeat argc (vm/stack-pop)))
                _ (update-val :_output #(concat % args))
                _ (vm/reg-set :r0 (p/vm-nil))]
               nil))}

  bif/tads-net)

(defn trace-host []
  (TraceHost.))

(defn trace-step
  "Execute the op code referenced by the ip register."
  [host]
  (m/do-vm
   [ip (vm/reg-get :ip)
    [op args len] (vm/parse-op-at-ip)
    _ (update-val :ip (partial + len))
    stack (fetch-val :stack)
    _ (update-val :_trace #(concat % [{:op op :args args :pre stack :ip ip}]))
    ret (apply (:run-fn op) (cons host (vals args)))]
   ret))

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
    (let [host (trace-host)
          m1 (m/exec-vm (vm/enter host) m0)
          m2 (assoc m1 :_trace [])
          stepper (trace-step host)]

      (loop [s m1]
        (let [[r s+] (try
                       (m/run-vm stepper s)
                       (catch Exception e
                         (spit (str "test/t3chnique/out/" name ".err")
                               (str (format-trace s) "\nexception: " e))
                         (throw e)))]
          (if r
            [r s+]
            (recur s+)))))))

(defn test [name]
  (let [resource-name (str name ".t3")
        [r s] (trace-execution resource-name)
        _ (println "Return was " r)
        trc (str (format-trace s)
                 (when r (str "\nreturn " (p/mnemonise r))))]
    (spit (str "test/t3chnique/out/" resource-name ".trc") trc)))

(defn compare-trace [name]
  (let [new (str "test/t3chnique/out/" name ".t3.trc")
        old (str "test/t3chnique/out/" name ".t3.good")]
    (= (string/trim (slurp new)) (string/trim (slurp old)))))


(fact
  (let [s (test "arith")]
    (compare-trace "arith") => true
    ))

(fact
  (let [s (test "basic")]
    (compare-trace "basic") => true
    ))

