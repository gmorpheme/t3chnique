(ns t3chnique.main
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]])
  (:require [t3chnique.vm :as vm]
            [t3chnique.image :as im]
            [t3chnique.ber :as ber]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(declare entp dis output)

(defn -main [& args]
  (let [[opts args h] (cli args
                           ["-r" "--resource" "Load game from resource instead of file system." :flag true]
                           ["-e" "--entp" "Output entry point information" :flag true]
                           ["-s" "--state" "Output initial vm state (without binary entries)" :flag true]
                           ["-d" "--disassemble" "Output disassembled code for function at specified address" :parse-fn #(Integer. %)]
                           ["-h" "--help" "Output this help text." :flag true])
        game (first args)
        image (if (:resource opts)
                (im/parse-resource game)
                (im/parse-file game))
        state (vm/vm-from-image image)]
    (cond
     (:entp opts) (entp image)
     (:state opts) (output "Initial VM State" state)
     (:disassemble opts) (dis (:disassemble opts) state))))

(defn output [title m]
  (println "=======================================")
  (println title)
  (println "=======================================")
  (pp/pprint m))

(defn format-op [{:keys [offset mnemonic args]}]
  (format "%8d  %s %s" offset mnemonic (apply str args)))

(defn entp
  "Output entry point information from image"
  [image]
  (let [e (first (filter #(= (:id %) "ENTP") image))]
    (output "ENTP Block" e)))

(defn dis1
  "Disassemble single instruction from buffer, incrementing pointer."
  ([buf]
     (let [offs (.position buf)]
       (dis1 buf offs)))
  ([buf offs]
     (let [_ (.position buf offs)
           opcode (ber/read-ubyte buf)]
       (when-let [op (@vm/table opcode)]
         {:offset offs
          :opcode opcode
          :mnemonic (vm/mnemonic op)
          :args (ber/parse (vm/parse-spec op) buf)}))))

(defn disn
  [buf start end]
  (do
    (.position buf start)
    (loop [ops []]
      (if (or (zero? end) (< (.position buf) end))
        (if-let [d (dis1 buf)]
          (recur (conj ops d))
          ops)
        ops))))

(defn dis
  "Print disassembled code at offset"
  [offs state]
  (let [[mh s] ((vm/get-method-header offs) state)
        code (+ offs (:code-offset mh))
        {:keys [etable-offset dtable-offset]} mh
        etable (if (pos? etable-offset) (+ offs etable-offset) 0)
        dtable (if (pos? dtable-offset) (+ offs dtable-offset) 0)
        end (if (zero? etable) dtable etable)]
    (println "Method\n")
    (println (format "Offsets (code/etable/dtable): %d/%d/%d\n" code etable dtable))
    (println (format "Locals: %d Slots: %d Params: %d Opt Params: %d\n"
                     (:local-variable-count mh)
                     (:max-slots mh)
                     (:param-count mh)
                     (:opt-param-count mh)))
    (doseq [op (apply disn (conj (vm/offset state code) end))]
      (println (format-op op)))
    (when (pos? etable)
      (println)
      (let [[b o] (vm/offset state etable)
            _ (.position b 0)
            erecords (im/read-exception-table b)]
        (doseq [e erecords]
          (println (format "%8d - %8d: oid: %d offset: %d"
                           (:first-offset e)
                           (:last-offset e)
                           (:oid e)
                           (:handler-offset e))))))))
