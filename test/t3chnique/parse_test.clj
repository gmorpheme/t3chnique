(ns t3chnique.test.parse-test
  (:use [t3chnique.parse]
        [midje.sweet])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

(defn init [buf] {:parse-state [buf 0]})

(defn make-buf [bytes]
  (ByteBuffer/wrap
   (byte-array
    (map unchecked-byte bytes))))

(defn utf8-buf [str]
  (make-buf (seq (.getBytes str "utf-8"))))

(defn utf8-pbuf [c str]
  (make-buf (cons c (seq (.getBytes str "utf-8")))))

(facts
  (parse (utf8 5) (utf8-buf "hello")) => "hello"
  (parse (prefixed-utf8) (utf8-pbuf 5 "hello")) => "hello"
  (parse (times 4 (ubyte)) (make-buf [0x02 0x01 0x02 0x02]) => [2 1 2 1]))