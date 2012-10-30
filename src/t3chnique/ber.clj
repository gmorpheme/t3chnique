(ns t3chnique.ber
  (:require [nio.core :as nio]
            [clojure.java.io :as io]
            [t3chnique.primitive :as prim])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer]))

(defn slice 
  "Slice a new buffer off the base of count bytes"
  ([b]
     (let [slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)]
       slice))
  ([b count]
     (let [save (.limit b)
           _ (.limit b (+ count (.position b)))
           slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)
           _ (.position b (.limit b))
           _ (.limit b save)]
       slice)))

(defn read-sbyte
  "Read a singly ubyte from the buffer (advancing position 1)"
  [b]
  (.get b))

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

(defn read-utf8 
  "Read to limit at utf-8"
  [b count]
  (let [utf8 (Charset/forName "utf-8")
        decoder (.newDecoder utf8)
        slice (slice b count)]
    (String. (.array (.decode decoder slice)))))

(defn read-pref-utf8
  "Read utf-8 prefixed with length as ubyte"
  [b]
  (let [count (read-ubyte b)]
    (read-utf8 b count)))

(declare read-item)

(defn read-data-holder
  "Read a data holder from the buffer (advancing position 5"
  [b]
  (let [pos (.position b)
        typeid (.get b)
        encoding (:encoding (prim/primitive typeid))
        value (if (nil? encoding) nil (read-item encoding b))
        _ (.position b (+ pos 5))]
    (prim/typed-value typeid value)))

(defn read-byte-array [b count]
  (let [dest (byte-array count)
        _ (.get b dest 0 count)]
    dest))

(defn show-bytes
  "Peek into the buffer to display the next few bytes as hex."
  ([b count]
     (let [b' (.slice b)]
       (apply str 
              (for [i (range count)]
                (format "%02x" (.get b'))))))
  ([b]
     (show-bytes b 10)))

(defn read-item [type-sym buf]
  (condp = type-sym
    :uint2 (read-uint2 buf)
    :int2 (read-int2 buf)
    :uint4 (read-uint4 buf)
    :int4 (read-int4 buf)
    :ubyte (read-ubyte buf)
    :sbyte (read-sbyte buf)
    :data-holder (read-data-holder buf)
    :pref-utf8 (read-pref-utf8 buf)
    (read-utf8 buf (second type-sym))))

(defn parse
  "e.g. (parse [:uint2 :foo :uint4 :bar [:utf8 4] :str] buf) returns {:foo 123 :bar 1234 :str \"blah\"}"
  [spec buf]
  (reduce merge (for [[t n] (partition 2 spec)]
                  (sorted-map (keyword n) (read-item t buf)))))

(defn read-method-header [buf size]
  (let [b (slice buf size)]
    (parse [:ubyte :param-count
            :ubyte :opt-param-count
            :uint2 :local-variable-count
            :uint2 :max-slots
            :uint2 :exception-table-offset
            :uint2 :debugging-records-offset] b)))

