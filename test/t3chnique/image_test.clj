(ns t3chnique.image-test
  (:use clojure.java.io
        t3chnique.image
        midje.sweet)
  (:import [java.nio ByteBuffer]))

(fact
 (parse-resource "Elysium.t3") => #(not (empty? %)))

(fact
 (parse-resource "ditch3.t3") => #(not (empty? %)))

(fact
  (let [new-buf (fn [byte-seq f]
                  (let [buf (ByteBuffer/wrap
                             (byte-array
                              (map unchecked-byte byte-seq)))]
                    (f buf)
                    (.rewind buf)))
        to-seq (fn [buf]
                 (.rewind buf)                 
                 (let [arr (byte-array (.remaining buf))]
                   (.get buf arr 0 (.remaining buf))
                   (seq arr)))]
    (to-seq (new-buf       [0xff 0xee 0xdd 0xcc 0xbb 0xaa 0x99 0x88 0x77 0x66] #(de-xor % 0xff)))
    => (map unchecked-byte [0x00 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99])))

