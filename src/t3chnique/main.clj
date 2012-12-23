(ns t3chnique.main
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]])
  (:require [t3chnique.vm :as vm]
            [t3chnique.image :as im]
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

(defn entp
  "Output entry point information from image"
  [image]
  (let [e (first (filter #(= (:id %) "ENTP") image))]
    (output "ENTP Block" e)))

(defn dis
  "Print disassembled code at offset"
  [offset state]
  (println offset)
  (let [[mh s] ((vm/get-method-header offset) state)]
    (output "Method Header" mh)))

(comment
  (defn dis1
    "Disassemble single instruction from buffer, incrementing pointer."
    [buf]
    (let [offset (.position buf)
          opcode (ber/read-ubyte buf)
          op (@table opcode)
          _ (if (nil? op) (throw (RuntimeException. (str "No op for opcode " opcode))))
          mnemonic (mnemonic op)
          spec (parse-spec op)
          args (ber/parse spec buf)]
      {:offset offset :opcode opcode :mnemonic mnemonic :args args})))