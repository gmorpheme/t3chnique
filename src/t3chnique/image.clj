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

(defmulti read-block (fn [block buf] (:id block)))

(defn parse-image [b]
  (let [[version timestamp] (read-sig b)]
    (loop [buf b blocks []]
      (let [header (read-block-header b)
            data (read-block header b)
            block (merge header data)]
        (if (nil? data)
          [blocks header]
          (recur buf (conj blocks block)))))))

(defn load-image-file [f]
  (let [buf (nio/mmap f)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)]
    buf))

(defmethod read-block "ENTP" [{size :size} buf]
  (let [slice (ber/slice buf size)
        
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
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)]
    {:entries (loop [remaining (dec entry-count) entries {}]
                (let [value (ber/read-data-holder slice)
                      chars (ber/read-ubyte slice)
                      name (ber/read-utf8 slice chars)]
                  (if (pos? remaining)
                    (recur (dec remaining) (assoc entries name value))
                    entries)))}))

(defmethod read-block "FNSD" [{size :size} buf]
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)]
    {:entries (loop [remaining (dec entry-count) entries []]
                (let [chars (ber/read-ubyte slice)
                      name (ber/read-utf8 slice chars)]
                  (if (pos? remaining)
                    (recur (dec remaining) (conj entries name))
                    entries)))}))

(defmethod read-block "CPDF" [{size :size} buf]
  (let [slice (ber/slice buf size)]
    (ber/parse [:uint2 :pool-id :uint4 :page-count :uint4 :page-size] slice)))

(defmethod read-block "CPPG" [{size :size} buf]
  (let [slice (ber/slice buf size)
        m (ber/parse [:uint2 :pool-id :uint4 :pool-index :ubyte :xor-mask] slice)
        bytes (ber/slice slice (- size 7))]
    (assoc m :bytes bytes)))

(defmethod read-block "MCLD" [{size :size} buf]
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)]
    {:entries  (loop [remaining (dec entry-count) entries []]
                 (let [offset (ber/read-uint2 slice)
                       chars (ber/read-ubyte slice)
                       name (ber/read-utf8 slice chars)
                       pid-count (ber/read-uint2 slice)
                       pid-size (ber/read-uint2 slice)
                       pids (reduce (fn [x y] (conj x (ber/read-uint2 slice))) [] (range pid-count))]
                   (if (pos? remaining)
                     (recur (dec remaining) (conj entries {:name name :pids pids}))
                     entries)))}))

(defmethod read-block :default [block buf]
  )