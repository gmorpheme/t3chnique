(ns t3chnique.image
  (:require [t3chnique.ber :as ber])
  (:require [nio.core :as nio]
            [clojure.java.io :as io])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

(set! *warn-on-reflection* true)

(defn read-sig [^ByteBuffer b]
  (let [sig (ber/read-utf8 b 11)
        ver (ber/read-uint2 b)
        _ (.position b 45)
        timestamp (ber/read-utf8 b 24)]
    [ver timestamp]))

(defn read-block-header [^ByteBuffer b]
  (let [m (ber/parse [[:utf8 4] :id :uint4 :size :uint2 :flags] b)]
    (assoc m :mandatory (= (bit-and 1 (:flags m)) 1))))

(defmulti read-block (fn [block buf] (:id block)))

(defn parse-image [^ByteBuffer b]
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

(defn parse-file [name]
  (let [f (io/file name)
        buf (load-image-file f)]
    (parse-image buf)))

(defn parse-resource [name]
  (let [f (io/file (io/resource name))
        buf (load-image-file f)]
    (parse-image buf)))

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
    {:entry-count entry-count
     :entries (loop [n 0 entries []]
                (if (< n entry-count)
                  (let [value (ber/read-data-holder slice)
                        chars (ber/read-ubyte slice)
                        name (ber/read-utf8 slice chars)]
                    (recur (inc n) (conj entries {:name name :value value})))
                  entries))}))

(defmethod read-block "FNSD" [{size :size} buf]
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)]
    {:entry-count entry-count
     :entries (loop [n 0 entries []]
                (if (< n entry-count)
                  (let [chars (ber/read-ubyte slice)
                        name (ber/read-utf8 slice chars)]
                    (recur (inc n) (conj entries name)))
                  entries))}))

(defmethod read-block "CPDF" [{size :size} buf]
  (let [slice (ber/slice buf size)]
    (ber/parse [:uint2 :pool-id :uint4 :page-count :uint4 :page-size] slice)))

(defn de-xor
  [^ByteBuffer buf mask]
  (let [l (.remaining buf)
        a (byte-array l)]
    (.mark buf)
    (.get buf ^bytes a 0 l)
    (let [a' (amap ^bytes a i ret (unchecked-byte (bit-xor mask (aget ^bytes a i))))]
      (.reset buf)
      (.put buf ^bytes a' 0 l))))

(defmethod read-block "CPPG" [{size :size} buf]
  (let [slice (ber/slice buf size)
        m (ber/parse [:uint2 :pool-id :uint4 :pool-index :ubyte :xor-mask] slice)
        bytes (ber/slice slice (- size 7))
        _ (de-xor bytes (:xor-mask m))]
    (assoc m :bytes bytes)))

(defmethod read-block "MCLD" [{size :size} buf]
  (let [slice (ber/slice buf size)
        entry-count (ber/read-uint2 slice)]
    {:entry-count entry-count
     :entries  (loop [n 0 entries []]
                 (if (< n entry-count)
                   (let [offset (ber/read-uint2 slice)
                         chars (ber/read-ubyte slice)
                         name (ber/read-utf8 slice chars)
                         pid-count (ber/read-uint2 slice)
                         pid-size (ber/read-uint2 slice)
                         pids (reduce (fn [x y] (conj x (ber/read-uint2 slice))) [] (range pid-count))]
                     (recur (inc n) (conj entries {:name name :pids pids})))
                   entries))}))

(defmethod read-block "OBJS" [{size :size} buf]
  (let [slice (ber/slice buf size)
        object-count (ber/read-uint2 slice)
        mcld-index (ber/read-uint2 slice)
        flags (ber/read-uint2 slice)
        large (= (bit-and flags 1) 1)
        transient (= (bit-and flags 2) 1)]
    {:object-count object-count
     :mcld-index mcld-index
     :flags flags
     :large large
     :transient transient
     :objects (loop [n 0 objects []]
                (if (< n object-count)
                  (let [oid (ber/read-uint4 slice)
                        count ((if large ber/read-uint4 ber/read-uint2) slice)
                        bytes (ber/slice slice count)]
                    (recur (inc n) (conj objects {:oid oid :bytes bytes})))
                  objects))}))

(defmethod read-block "MRES" [{size :size} ^ByteBuffer buf]
  (let [slice ^ByteBuffer (ber/slice buf size)
        entry-count (ber/read-uint2 slice)
        toc (loop [n 0 entries []]
              (if (< n entry-count)
                (let [offset (ber/read-uint4 slice)
                      size (ber/read-uint4 slice)
                      chars (ber/read-ubyte slice)
                      barr (ber/read-byte-array slice chars)
                      xbytes (amap ^bytes barr i _ (byte (bit-xor (aget ^bytes barr i) (byte -1))))
                      name (String. ^bytes xbytes "ascii")]
                  (recur (inc n) (conj entries {:size size :offset offset :name name})))
                entries))
        entries (reduce
                 (fn [l m]
                   (conj l
                         (assoc m :bytes (do (.position slice (:offset m)) (ber/slice slice (:size m))))))
                 []
                 toc)]
    {:entry-count entry-count
     :entries entries}))

(defmethod read-block "EOF " [{size :size} buf] {})

(defmethod read-block :default [block buf] {:unknown (:id block)} )

(defn read-method-header [buf size]
  (let [b (ber/slice buf size)]
    (assoc 
        (ber/parse [:ubyte :param-count
                    :ubyte :opt-param-count
                    :uint2 :local-variable-count
                    :uint2 :max-slots
                    :uint2 :etable-offset
                    :uint2 :dtable-offset] b)
      :code-offset size)))

(defn read-exception-handler [buf]
  (ber/parse [:uint2 :first-offset
              :uint2 :last-offset
              :uint4 :oid
              :uint2 :handler-offset] buf))

(defn read-exception-table [buf]
  (let [count (ber/read-uint2 buf)
        etable (loop [n 0 entries []]
                 (if (< n count)
                   (recur (inc n) (conj entries (read-exception-handler buf)))
                   entries))]
    etable))

;;

