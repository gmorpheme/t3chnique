(ns t3chnique.parse
  (:use [clojure.algo.monads])
  (:require [nio.core :as nio]
            [clojure.java.io :as io])
  (:require [t3chnique.primitive :as prim])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

(defn slice 
  "Slice a new buffer off the base of count bytes"
  ([^ByteBuffer b]
     (let [slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)]
       slice))
  ([^ByteBuffer b count]
     (slice b (.position b) count))
  ([^ByteBuffer b offset count]
     {:pre [(not (nil? b)), (not (neg? offset)), (pos? count)]}
     (let [[pos limit] [(.position b) (.limit b)]]
       (try
         (.position b offset)
         (.limit b (+ offset count))
         (doto (.slice b)
           (.order ByteOrder/LITTLE_ENDIAN))
         (finally 
          (.limit b limit)
          (.position b pos))))))

(defprotocol ByteSource
  (read-sbyte [self idx])
  (read-ubyte [self idx])
  (read-int2 [self idx])
  (read-uint2 [self idx])
  (read-int4 [self idx])
  (read-uint4 [self idx])
  (read-utf8 [self idx count])
  (read-bytes [self idx count]))

(extend-protocol ByteSource
  ByteBuffer
  (read-sbyte [b idx] (.get b idx))
  (read-ubyte [b idx] (bit-and (.get b idx) 0xff))
  (read-int2 [b idx] (.getShort b idx))
  (read-uint2 [b idx] (bit-and (.getShort b idx) 0xffff))
  (read-int4 [b idx] (.getInt b idx))
  (read-uint4 [b idx] (bit-and (.getInt b idx) 0xffffffff))
  (read-utf8 [b idx count] (let [utf8 (Charset/forName "utf-8")
                                 decoder (.newDecoder utf8)
                                 slice (slice b idx count)]
                             (String. (.array (.decode decoder slice)))))
  (read-bytes [b idx count] (let [a (byte-array count)]
                              (.position b idx)
                              (.get b ^bytes a 0 count)
                              a)))

(def byteparser-m state-m)

(defmacro defbasic [n f s]
  `(defn ~n []
     (domonad byteparser-m
      [[b# i#] (fetch-state)
       _# (set-state [b# (+ ~s i#)])]
      (~f b# i#))))

(defn init-state [buf] [buf 0])

(defn parse [parser buffer]
  (let [st (init-state buffer)]
    (first (parser st))))

(with-monad byteparser-m
  
  (defn skip [n]
    (domonad
     [[b i] (fetch-state)
      _ (set-state [b (+ i n)])]
     nil))

  (defn skip-to [n]
    (domonad
     [[b i] (fetch-state)
      _ (set-state [b n])]
     nil))

  (defn within [count parser]
    (domonad
     [[_ start] (fetch-state)
      value parser
      [_ end] (fetch-state)
      _ (skip (- count (- end start)))]
     value))

  (defbasic ubyte read-ubyte 1)
  (defbasic sbyte read-sbyte 1)
  (defbasic uint2 read-uint2 2)
  (defbasic int2 read-int2 2)
  (defbasic uint4 read-uint4 4)
  (defbasic int4 read-int4 4)

  (defn utf8 [byte-count]
    (domonad
     [[b i] (fetch-state)
      _ (set-state [b (+ byte-count i)])]
     (read-utf8 b i byte-count)))

  (defn prefixed-utf8 []
    (domonad
     [count (ubyte)
      value (utf8 count)]
     value))

  (defn binary [n]
        (domonad
         [[b i] (fetch-state)
          _ (set-state [b (+ n i)])]
     (read-bytes b i n)))

  (defn xor-bytes [n mask]
    (domonad
     [[b i] (fetch-state)
      _ (set-state [b (+ n i)])]
     (let [a (read-bytes b i n)]
       (amap ^bytes a i _ (unchecked-byte (bit-xor mask (aget ^bytes a i)))))))

  (defn prefixed-xor-ascii []
    (domonad
     [n (ubyte)
      a (binary n)]
     (let [xbytes (amap ^bytes a i _ (unchecked-byte (bit-xor 0xff (aget ^bytes a i))))]
       (String. ^bytes xbytes "ascii"))))

  (defn record [& args]
    (let [[keywords parsers] (map #(take-nth 2 %) [args (rest args)])]
      (domonad
       [vals (m-seq parsers)]
       (zipmap keywords vals))))

  (defn times [n parser]
    (m-seq (repeat n parser)))

  (defn parse-until [p parser]
    (domonad
     [val parser
      more (m-when (not (p val)) (parse-until p parser))]
     (cons val more)))

  (defn tagged-parser [kw]
    (cond
     (= kw :uint2) (uint2)
     (= kw :int2) (int2)
     (= kw :uint4) (uint4)
     :else nil))
  
  (defn data-holder []
    (domonad
     [typeid (ubyte)
      value (within 4 (tagged-parser (:encoding (prim/primitive typeid))))]
     (prim/typed-value typeid value)))
  
  (defn lst []
    (domonad
     [n (uint2)
      v (times n (data-holder))]
     v))
  
  (defn signature []
    (domonad
     [sig (utf8 11)
      ver (uint2)
      _ (skip 32)
      timestamp (utf8 24)]
     [ver timestamp sig]))
  
  (defn block-header []
    (record :id (utf8 4) :size (uint4) :flags (uint2)))

  (defmulti block-body :id)
  
  (defmethod block-body "ENTP" [_]
    (apply record
           [:entry-point-offset (uint4)
            :method-header-size (uint2)
            :exception-table-entry-size (uint2)
            :debugger-line-table-entry-size (uint2)
            :debug-table-header-size (uint2)
            :debug-table-local-symbol-record-header-size (uint2)
            :debug-records-version-number (uint2)])) ;todo frame
                                        ;header size

  (defmethod block-body "SYMD" [_]
    (domonad
     [n (uint2)
      entries (times n (record :value (data-holder) :name (prefixed-utf8)))]
     {:entry-count n :entries entries}))

  (defmethod block-body "FNSD" [_]
    (domonad
     [n (uint2)
      entries (times n (prefixed-utf8))]
     {:entry-count n :entries entries}))

  (defmethod block-body "CPDF" [_]
    (record :pool-id (uint2) :page-count (uint4) :page-size (uint4)))

  (defmethod block-body "CPPG" [{size :size}]
    (domonad
     [m (record :pool-id (uint2) :pool-index (uint4))
      mask (ubyte)
      bytes (xor-bytes (- size 7) mask)]
     (assoc m :bytes bytes)))

  (defmethod block-body "MCLD" [_]
    (domonad
     [n (uint2)
      entries (times n
                     (record :offset (uint2)
                             :name (prefixed-utf8)
                             :pids (domonad [pid-count (uint2)
                                             pid-size (uint2)
                                             pids (times pid-count (uint2))] pids)))]
     {:entry-count n :entries entries}))

  (defmethod block-body "OBJS" [_]
    (domonad
     [n (uint2)
      mcld-index (uint2)
      flags (uint2)
      :let [large (= (bit-and flags 1) 1)
            transient (= (bit-and flags 2) 1)]
      objects (times n (domonad
                        [oid (uint4)
                         count (if large (uint4) (uint2))
                         bytes (binary count)]
                        {:oid oid :bytes bytes}))]
     {:mcld-index mcld-index
      :large large
      :transient transient
      :objects objects}))

  (defn binary-at [idx size]
    (domonad
     [_ (skip-to idx)
      val (binary size)]
     val))
  
  (defmethod block-body "MRES" [_]
    (domonad
     [[b start] (fetch-state)
      n (uint2)
      toc (times n (record :offset (uint4) :size (uint4) :name (prefixed-xor-ascii)))
      :let [names (map :name toc)]
      entries (m-seq (map (fn [{:keys [offset size]}] (binary-at (+ offset start) size)) toc))]
     (zipmap names entries)))

  (defmethod block-body "EOF " [_]
    (m-result {}))

  (defn block []
    (domonad
     [[b i] (fetch-state)
      header (block-header)
      data (within (:size header)
                   (block-body header))]
     (do (println (:id header) " at " i)
         (merge header data))))
  
  (defn image []
    (domonad
     [[version timestamp] (signature)
      blocks (parse-until #(= (:id %) "EOF ") (block))]
     blocks)))

(defn load-image-file [f]
  (let [buf (nio/mmap f)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)]
    buf))

(defn parse-file [name]
  (let [f (io/file name)
        buf (load-image-file f)]
    (parse (image) buf)))

(defn parse-resource [name]
  (let [f (io/file (io/resource name))
        buf (load-image-file f)]
    (parse (image) buf)))

;; testing functions

(defn make-buf [bytes]
  (ByteBuffer/wrap
   (byte-array
    (map unchecked-byte bytes))))

(defn utf8-buf [str]
  (make-buf (seq (.getBytes str "utf-8"))))

(defn utf8-pbuf [c str]
  (make-buf (cons c (seq (.getBytes str "utf-8")))))
