(ns t3chnique.image
  (:require [t3chnique.ber :as ber])
  (:require [nio.core :as nio]
            [clojure.java.io :as io])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer]))

(defn read-sig [b]
  (let [sig (ber/read-utf8 b 11)
        ver (ber/read-uint2 b)
        _ (.position b 45)
        timestamp (ber/read-utf8 b 24)]
    [ver timestamp]))

(defn read-block-header [b]
  (let [m (ber/parse [[:utf8 4] :id :uint4 :size :uint2 :flags] b)]
    (assoc m :mandatory (= (bit-and 1 (:flags m)) 1))))

(defn parse-image [b]
  (let [_ (read-sig b)]
    (loop [buf b blocks []]
      (let [header (read-block-header b)
            data (read-block header b)]
        (recur buf (conj blocks (merge header data)))))))

(defn load-image-file [f]
  (let [buf (nio/mmap f)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)
        version (read-sig buf)]
    buf))

(defmulti read-block (fn [block buf] (:id block)))

(defmethod read-block "ENTP" [{size :size} buf]
  (let [slice (slice buf size)
        _ (println slice)
        spec (conj [:uint4 :entry-point-offset
                    :uint2 :method-header-size
                    :uint2 :exception-table-entry-size
                    :uint2 :debugger-line-table-entry-size
                    :uint2 :debug-table-header-size
                    :uint2 :debug-table-local-symbol-record-header-size
                    :uint2 :debug-records-version-number]
                   (when (> size 16) [:uint2 :debug-table-frame-header-size]))]
    (ber/parse spec slice)))

(defmethod read-block "SYMD" [{size :size} buf]
  (let [slice (slice buf size)
        entries (ber/read-uint2 buf)]
    ))

(defmethod read-block :default [block buf]
  )