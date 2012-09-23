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
  (let [_ (.position b 0)
        [version timestamp] (read-sig b)]
    (loop [buf b blocks []]
      (let [header (read-block-header b)
            data (read-block header b)
            block (merge header data)]
        (if (= "EOF " (:id block))
          (conj blocks block)
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

(defmethod read-block "OBJS" [{size :size} buf]
  (let [slice (ber/slice buf size)
        object-count (ber/read-uint2 slice)
        mcld-index (ber/read-uint2 slice)
        flags (ber/read-uint2 slice)
        large (= (bit-and flags 1) 1)
        transient (= (bit-and flags 2) 1)]
    {:objects (loop [remaining (dec object-count) objects []]
                (let [oid (ber/read-uint4 slice)
                      count ((if large ber/read-uint4 ber/read-uint2) slice)
                      bytes (ber/slice slice count)]
                  (if (pos? remaining)
                    (recur (dec remaining) (conj objects {:oid oid :bytes bytes}))
                    objects)))}))

(defmethod read-block "MRES" [{size :size} buf]
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)
        toc (loop [remaining (dec entry-count) entries []]
              (let [offset (ber/read-uint4 slice)
                    size (ber/read-uint4 slice)
                    chars (ber/read-ubyte slice)
                    barr (ber/read-byte-array slice chars)
                    xbytes (amap ^bytes barr i _ (byte (bit-xor (aget ^bytes barr i) (byte -1))))
                    name (String. xbytes "ascii")]
                (if (pos? remaining)
                  (recur (dec remaining) (conj entries {:size size :offset offset :name name}))
                  entries)))
        entries (reduce
                 (fn [l m]
                   (conj l
                         (assoc m :bytes (do (.position slice (:offset m)) (ber/slice slice (:size m))))))
                 []
                 toc)]
    {:entries entries}))

(defmethod read-block "EOF " [{size :size} buf] {})

(defmethod read-block :default [block buf] {:unknown (:id block)} )