(ns t3chnique.main
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]])
  (:require [t3chnique.vm :as vm]
            [t3chnique.image :as im]
            [t3chnique.ber :as ber]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import [java.nio Buffer]))

(declare entp dis object constant-string constant-list output)

(defn -main [& args]
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
                (im/parse-resource game)
                (im/parse-file game))
        state (vm/vm-from-image image)]
    (time 
     (cond
      (:entp opts) (entp image)
      (:state opts) (output "Initial VM State" state)
      (:disassemble opts) (dis (:disassemble opts) state)
      (:object opts) (object (:object opts) state)
      (:constant-string opts) (constant-string (:constant-string opts) state)
      (:constant-list opts) (constant-list (:constant-list opts) state)))))

(defn output [title m]
  (println "=======================================")
  (println title)
  (println "=======================================")
  (pp/pprint m))

(defn format-op [{:keys [address offset mnemonic args]}]
  (format "%8d %8d  %s %s" address offset mnemonic (apply str args)))

(defn entp
  "Output entry point information from image"
  [image]
  (let [e (first (filter #(= (:id %) "ENTP") image))]
    (output "ENTP Block" e)))

(defn- dis1
  "Disassemble single instruction from buffer, incrementing pointer."
  [buffer-addr method-addr ^Buffer buf ptr]
  (let [_ (.position buf ptr)
        opcode (ber/read-ubyte buf)]
    (when-let [op (@vm/table opcode)]
      [{:address (+ buffer-addr ptr)
        :offset (- (+ buffer-addr ptr) method-addr)
        :opcode opcode
        :mnemonic (vm/mnemonic op)
        :args (ber/parse (vm/parse-spec op) buf)}
       (.position buf)])))

(defn dis-method
  [buffer-addr method-addr ^Buffer buf start end]
  (loop [ops [] ptr start]
    (if (or (zero? end) (< (.position buf) end))
      (if-let [[d nextptr] (dis1 buffer-addr method-addr buf ptr)]
        (recur (conj ops d) nextptr)
        ops)
      ops)))

(defn dis
  "Print disassembled code at offset"
  [offs state]
  (let [[mh s] ((vm/get-method-header offs) state)
        {:keys [code-offset etable-offset dtable-offset]} mh
        code-addr (+ offs code-offset)
        etable-addr (if (pos? etable-offset) (+ offs etable-offset) 0)
        dtable-addr (if (pos? dtable-offset) (+ offs dtable-offset) 0)
        end (if (zero? etable-addr) dtable-addr etable-addr)]

    ;; method header
    (println (format "Method Header @ %d\n" offs))
    (println (format "Offsets (code/etable/dtable): %d/%d/%d\n" code-offset etable-offset dtable-offset))
    (println (format "Locals: %d Slots: %d Params: %d Opt Params: %d\n"
                     (:local-variable-count mh)
                     (:max-slots mh)
                     (:param-count mh)
                     (:opt-param-count mh)))

    ;; disassembled operations
    (let [method-addr offs
          [buf ptr] (vm/offset state code-addr)
          buffer-addr (- code-addr ptr)
          [_ endptr] (vm/offset state end)]
      (println (format "Code @ %d\n" code-addr))
      (doseq [op (dis-method buffer-addr method-addr buf ptr endptr)]
        (println (format-op op))))

    ;; exception table
    (when (pos? etable-offset)
      (println (format "\nException Table @ %d\n" etable-addr))
      (let [[^Buffer b o] (vm/offset state etable-addr)
            _ (.position b o)
            erecords (im/read-exception-table b)]
        (doseq [e erecords]
          (println (format "%8d - %8d: oid: %d offset: %d"
                           (:first-offset e)
                           (:last-offset e)
                           (:oid e)
                           (:handler-offset e))))))))

(defn object
  "Dump out object information for object with specified oid."
  [oid state]
  (output (str "Object " oid) (get (:objs state) oid)))

(defn constant-string
  "Dump out constant information for object at specified address in the constant pool."
  [addr state]
  (let [[^Buffer b o] (vm/const-offset state addr)]
    (.position b o)
    (println (format "String @ %d\n" addr))
    (println (ber/read-pref-utf8 b))))

(defn constant-list
  "Dump out constant list at specified address in the constant pool."
  [addr state]
  (let [[^Buffer b o] (vm/const-offset state addr)]
    (.position b o)
    (println (format "List @ %d\n" addr))
    (println (ber/read-list b))))