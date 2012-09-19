(ns t3chnique.ber
  (:require [nio.core :as nio]
             [clojure.java.io :as io])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer]))

(defn slice 
  "Slice a new buffer off the base of count bytes"
  [b count]
  (let [save (.limit b)
        _ (.limit b (+ count (.position b)))
        slice (.slice b)
        _ (.order slice ByteOrder/LITTLE_ENDIAN)
        _ (.position b (.limit b))
        _ (.limit b save)]
    slice))

(defn read-utf8 
  "Read to limit at utf-8"
  [b count]
  (let [utf8 (Charset/forName "utf-8")
        decoder (.newDecoder utf8)
        slice (slice b count)]
    (String. (.array (.decode decoder slice)))))

(defn read-uint2 
  "Read a single uint2 from the buffer (advancing position 2)"
  [b]
  (bit-and (.getShort b) 0xffff))

(defn read-uint4 
  "Read a single uint4 from the buffer (advancing position 4)"
  [b]
  (bit-and (.getInt b) 0xffffffff))

(defn read-data-holder
  "Read a data holder from the buffer (advancing position 5")

(defn read-item [type-sym buf]
  (condp = type-sym
    :uint2 (read-uint2 buf)
    :uint4 (read-uint4 buf)
    (read-utf8 buf (second type-sym))))

(defn parse
  "e.g. (parse [:uint2 :foo :uint4 :bar [:utf8 4] :str] buf) returns {:foo 123 :bar 1234 :str \"blah\"}"
  [spec buf]
  (reduce merge (for [[t n] (partition 2 spec)]
                  {(keyword n) (read-item t buf)})))

