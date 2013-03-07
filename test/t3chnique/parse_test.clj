(ns t3chnique.parse-test
  (:use [t3chnique.parse]
        [midje.sweet])
  (:require [t3chnique.primitive :as prim])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

;; testing functions

(defn make-buf [bytes]
  (ByteBuffer/wrap
   (byte-array
    (map unchecked-byte bytes))))

(defn utf8-buf [str]
  (make-buf (seq (.getBytes ^String str "utf-8"))))

(defn utf8-pbuf [c str]
  (make-buf (cons c (seq (.getBytes ^String str "utf-8")))))

(fact
  (parse (utf8 5) (utf8-buf "hello")) => "hello")

(fact
  (parse (prefixed-utf8) (utf8-pbuf 5 "hello")) => "hello")

(fact
  (parse (times 4 (ubyte)) (make-buf [0x02 0x01 0x02 0x01])) => [2 1 2 1])

(fact
  (parse (tagged-parser :uint2) (make-buf [0xcc 0xcc])) => 52428)

(fact
  (parse (data-holder) (make-buf [prim/vm-prop-id 0xcc 0xcc])) => {:type prim/vm-prop-id, :value 52428})

(fact
  (parse (lst) (make-buf [0x00 0x02 prim/vm-prop-id 0xbb 0xbb 0x00 0x00 prim/vm-prop-id 0xcc 0xcc 0x00 0x00]))
  => [{:type prim/vm-prop-id, :value 48059} {:type prim/vm-prop-id, :value 52428}])

(fact
  (parse (within 4 (uint2)) (make-buf [1 2 3 4])) => 258)

(fact
  (second (second ((within 4 (uint2)) (init-state (make-buf [1 2 3 4]))))) => 4)

(fact
  (parse (parse-until zero? (ubyte)) (make-buf [1 1 1 1 0 2 2])) => [1 1 1 1 0]) 

(fact
 (parse-resource "Elysium.t3") => #(not (empty? %)))

(fact
 (parse-resource "ditch3.t3") => #(not (empty? %)))
