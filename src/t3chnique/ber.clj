(ns t3chnique.ber
  (:require [nio.core :as nio]
            [clojure.java.io :as io]
            [t3chnique.primitive :as prim])
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

(defn read-ubyte
  "Read a singly ubyte from the buffer (advancing position 1)"
  [b]
  (bit-and (.get b) 0xff))

(defn read-uint2 
  "Read a single uint2 from the buffer (advancing position 2)"
  [b]
  (bit-and (.getShort b) 0xffff))

(defn read-int2
  "Read a single int2 from the buffer (advancing position 2)"
  [b]
  (.getShort b))

(defn read-uint4 
  "Read a single uint4 from the buffer (advancing position 4)"
  [b]
  (bit-and (.getInt b) 0xffffffff))

(defn read-int4
  "Read a single int4 from the buffer (advancing position 4)"
  [b]
  (.getInt b))

(declare read-item)

(defn read-data-holder
  "Read a data holder from the buffer (advancing position 5"
  [b]
  (let [pos (.position b)
        typeid (.get b)
        encoding (:encoding (prim/primitive typeid))
        value (read-item encoding b)
        _ (.position b (+ pos 5))]
    (prim/typed-value typeid value)))

(defn read-byte-array [b count]
  (let [dest (byte-array count)
        _ (.get b dest 0 count)]
    dest))

(defn read-item [type-sym buf]
  (condp = type-sym
    :uint2 (read-uint2 buf)
    :int2 (read-int2 buf)
    :uint4 (read-uint4 buf)
    :int4 (read-int4 buf)
    :ubyte (read-ubyte buf)
    :data-holder (read-data-holder buf)
    (read-utf8 buf (second type-sym))))

(defn parse
  "e.g. (parse [:uint2 :foo :uint4 :bar [:utf8 4] :str] buf) returns {:foo 123 :bar 1234 :str \"blah\"}"
  [spec buf]
  (reduce merge (for [[t n] (partition 2 spec)]
                  {(keyword n) (read-item t buf)})))

