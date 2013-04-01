(ns t3chnique.main
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]]
        [clojure.algo.monads :only [domonad fetch-state]]
        [clojure.main :only [repl]])
  (:require [t3chnique.vm :as vm]
            [t3chnique.parse :as parse]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import [java.nio Buffer]))

(declare entp dis object constant-string constant-list output vm-repl)

(defn -main [& args]
  (time
   (let [[opts args h] (cli args
                            ["-r" "--resource" "Load game from resource instead of file system." :flag true]
                            ["-e" "--entp" "Output entry point information" :flag true]
                            ["-s" "--state" "Output initial vm state (without binary entries)" :flag true]
                            ["-c" "--constant-string" "Output the string value at specified address in the constant pool" :parse-fn #(Integer. %)]
                            ["-l" "--constant-list" "Output the list value at specified address in the constant pool" :parse-fn #(Integer. %)]
                            ["-d" "--disassemble" "Output disassembled code for function at specified address" :parse-fn #(Integer. %)]
                            ["-o" "--object" "Output object information for specified object id" :parse-fn #(Integer. %)]
                            ["-h" "--help" "Output this help text." :flag true])
         game (first args)
         image (if (:resource opts)
                 (parse/parse-resource game)
                 (parse/parse-file game))
         state (vm/vm-from-image image)]
     (cond
      (:entp opts) (entp image)
      (:state opts) (output "Initial VM State" state)
      (:disassemble opts) (dis (:disassemble opts) state)
      (:object opts) (object (:object opts) state)
      (:constant-string opts) (constant-string (:constant-string opts) state)
      (:constant-list opts) (constant-list (:constant-list opts) state)
      :else (vm-repl state)))))

(defn output [title m]
  (println "=======================================")
  (println title)
  (println "=======================================")
  (pp/pprint m))

(defn entp
  "Output entry point information from image"
  [image]
  (let [e (first (filter #(= (:id %) "ENTP") image))]
    (output "ENTP Block" e)))

(defn method-limit [index {:keys [code-offset etable-offset dtable-offset]}]
  (+ index (max code-offset etable-offset dtable-offset)))

(defn disassemble-method [s m-addr]
  (-> (vm/offset s m-addr)
      ((domonad parse/byteparser-m
         [[_ i] (fetch-state)
          hdr (parse/method-header (:method-header-size s))
          ops (parse/repeat-up-to (max (+ i 100) (method-limit i hdr)) (vm/parse-op))]
         (merge hdr {:ops ops})))
      (first)))

(defn dis
  "Print disassembled code at offset"
  [state m-addr]
  (let [method (disassemble-method state m-addr)]

    ;; method header
    (println (format "Method Header @ %d\n" m-addr))
    (println (apply format
                    "Offsets (code/etable/dtable): %d/%d/%d\n"
                    ((juxt :code-offset :etable-offset :dtable-offset) method)))
    (println (apply format
                    "Locals: %d Slots: %d Params: %d Opt Params: %d\n"
                    ((juxt :local-variable-count 
                           :max-slots 
                           :param-count
                           :opt-param-count) method)))

    (println "Operations:")
    (doseq [[op args] (:ops method)]
      (println (:mnemonic op) " " (apply str (for [[k v] args] (str k ":" v " ")))))))

(defn object
  "Dump out object information for object with specified oid."
  [oid state]
  (output (str "Object " oid) (get (:objs state) oid)))

(defn constant-string
  "Dump out constant information for object at specified address in the constant pool."
  [addr state]
  (let [[b o] (vm/const-offset state addr)]
    (println (format "String @ %d\n" addr))
    (println (first ((parse/prefixed-utf8) [b o])))))

(defn constant-list
  "Dump out constant list at specified address in the constant pool."
  [addr state]
  (let [[b o] (vm/const-offset state addr)]
    (println (format "List @ %d\n" addr))
    (println (first ((parse/lst) [b o])))))

(defn vm-repl 
  "Run a REPL which applies the read fn to the state s."
  [s]
  (let [state (atom s)]
    (repl
     :init (fn []
             (use 't3chnique.vm)
             (use 'clojure.algo.monads))
     :eval (fn [form]
             (let [f (eval form)
                   [r s] (f @state)]
               (reset! state s)
               r)))))