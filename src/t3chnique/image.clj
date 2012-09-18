(ns t3chnique.image
  (:require [nio.core :as nio]
            [clojure.java.io :as io])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder]))

(defn load-image-file [f]
  (let [buf (nio/mmap f)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)
        version (read-sig)]
    buf))

(defn read-sig [b]
  (let [_ (.limit buf 11)
        sig (read-utf8 buf)
        _ (.limit buf 13)
        ver (read-uint2 buf)
        _ (.limit buf 69)
        _ (.position buf 45)
        timestamp (read-utf8 buf)]
    [ver timestamp]))

(defn slice [b count]
  "Slice a new buffer off the base of count bytes"
  (let [_ (.limit b (+ count (.position b)))
        slice (.slice b)
        _ (.position b (.limit b))]
    slice))

(defn read-block-header [b]
  (let [b (slice b 10)
        id (read-uint4 b)
        size (read-uint4 b)
        flags (read-uint2 b)
        mandatory (= (bit-and 1 flags) 1)]
    {:id id :size size :mandatory mandatory}))

(defn read-utf8 [b]
  "Read to limit at utf-8"
  (let [utf8 (Charset/forName "utf-8")
        decoder (.newDecoder utf8)]
    (String. (.array (.decode decoder b)))))

(defn read-uint2 
  "Read a single uint2 from the buffer (advancing position 2)"
  [b]
  (bit-and (.getShort b) 0xffff))

(defn read-uint4 
  "Read a single uint4 from the buffer (advancing position 4)"
  [b]
  (bit-and (long (.getInt b)) 0xffffffff))