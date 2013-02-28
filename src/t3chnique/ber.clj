(ns t3chnique.ber
  (:require [nio.core :as nio]
            [clojure.java.io :as io]
            [t3chnique.primitive :as prim])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

(set! *warn-on-reflection* true)

(defprotocol ByteSource
  (read-sbyte [self])
  (read-ubyte [self])
  (read-bytes [self count])
  (read-int2 [self])
  (read-uint2 [self])
  (read-int4 [self])
  (read-uint4 [self])
  (read-utf8 [self count]))

;; byte buffer based implementation
(defn slice 
  "Slice a new buffer off the base of count bytes"
  ([^ByteBuffer b]
     (let [slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)]
       slice))
  ([^ByteBuffer b count]
     (let [save (.limit b)
           _ (.limit b (+ count (.position b)))
           slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)
           _ (.position b (.limit b))
           _ (.limit b save)]
       slice)))

(extend-protocol ByteSource
  ByteBuffer
  (read-sbyte [b] (.get b))
  (read-ubyte [b] (bit-and (.get b) 0xff))
  (read-bytes [b count] (let [dest (byte-array count)]
                          (.get b dest 0 count)
                          dest))
  (read-int2 [b] (.getShort b))
  (read-uint2 [b] (bit-and (.getShort b) 0xffff))
  (read-int4 [b] (.getInt b))
  (read-uint4 [b] (bit-and (.getInt 0xffffffff)))
  (read-utf8 [b count] (let [utf8 (Charset/forName "utf-8")
                             decoder (.newDecoder utf8)
                             slice (slice b count)]
                         (String. (.array (.decode decoder slice))))))



;; functions built on top of the base protocols

(defn read-pref-utf8
  "Read utf-8 prefixed with length as ubyte"
  [b]
  (let [count (read-uint2 b)]
    (read-utf8 b count)))

(declare read-item)

(defn read-data-holder
  "Read a data holder from the buffer (advancing position 5"
  [^ByteSource b]
  (let [pos (.position b)
        typeid (.get b)
        encoding (:encoding (prim/primitive typeid))
        value (if (nil? encoding) nil (read-item encoding b))
        _ (.position b (+ pos 5))]
    (prim/typed-value typeid value)))

(defn read-list
  "Read prefix-counted list of data holders."
  [^ByteSource b]
  (loop [n (read-uint2 b) items []]
    (if (pos? n)
      (recur (dec n) (conj items (read-data-holder b)))
      items)))

(defn show-bytes
  "Peek into the buffer to display the next few bytes as hex."
  ([^ByteSource b count]
     (let [b' (.slice b)]
       (apply str 
              (for [i (range count)]
                (format "%02x" (.get b'))))))
  ([^ByteSource b]
     (show-bytes b 10)))

(defn read-item [type-sym ^ByteSource buf]
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
  "e.g. (parse [:uint2 :foo :uint4 :bar [:utf8 4] :str] buf) returns
 {:foo 123 :bar 1234 :str \"blah\"}"
  [spec buf]
  (reduce merge (for [[t n] (partition 2 spec)]
                  (sorted-map (keyword n) (read-item t buf)))))

