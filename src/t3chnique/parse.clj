(ns t3chnique.parse
  (:require [monads.core :refer :all]
            [monads.state :as st]
            [monads.util :refer [sequence-m mwhen]])
  (:require [nio.core :as nio]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer [trace]])
  (:require [t3chnique.primitive :as prim])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

(set! *warn-on-reflection* true)

(defn slice
  "Slice a new buffer off the base of count bytes"
  ([^ByteBuffer b]
     (let [slice (.slice b)
           _ (.order slice ByteOrder/LITTLE_ENDIAN)]
       slice))
  ([^ByteBuffer b count]
     (slice b (.position b) count))
  ([^ByteBuffer b offset count]
     {:pre [(not (nil? b)), (not (neg? offset)), (not (neg? count))]}
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
  (read-sbyte [b idx] (.get b ^int idx))
  (read-ubyte [b idx] (bit-and (.get b ^int idx) 0xff))
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

(extend-protocol ByteSource
  (Class/forName "[B")
  (read-sbyte [b idx]
    (aget ^bytes b idx))
  (read-ubyte [b idx]
    (bit-and 0xff (aget ^bytes b idx)))
  (read-int2 [b idx]
    (+ (bit-and 0xff (aget ^bytes b idx))
       (bit-shift-left (aget ^bytes b(inc idx)) 8)))
  (read-uint2 [b idx]
    (bit-and 0xffff (read-int2 b idx)))
  (read-int4 [b idx]
    (+ (bit-and 0xff (aget ^bytes b idx))
       (bit-shift-left (bit-and 0xff (aget ^bytes b(inc idx))) 8)
       (bit-shift-left (bit-and 0xff (aget ^bytes b(+ 2 idx))) 16)
       (bit-shift-left (aget ^bytes b(+ 3 idx)) 24)))
  (read-uint4 [b idx]
    (bit-and 0xffffffff (read-int4 b idx)))
  (read-utf8 [b idx count]
    (String. ^bytes b ^int idx ^int count "utf-8"))
  (read-bytes [b idx count]
    (into-array Byte/TYPE (take count (drop idx b)))))

(defmacro defbasic [name f sz]
  `(defn ~name []
     (mdo
      [b# i#] <- get-state
      (put-state [b# (+ ~sz i#)])
      (~f b# i#))))

(defn init-state [buf] [buf 0])

(defn parse [parser buffer]
  (st/eval-state parser (init-state buffer)))

(defn skip [n]
  (modify (fn [[b i]] [b (+ i n)])))

(defn skip-to [n]
  (modify (fn [[b i]] [b n])))

(def get-index
  (>>= get-state (fn [[_ i]] (return i))))

(defn within [n parser]
  (mdo
   start <- get-index
   val <- parser
   end <- get-index
   (skip (- n (- end start)))
   (return val)))

(defn repeat-up-to [limit parser]
  (mdo
   idx <- get-index
   (if (< idx limit)
     (mdo
      val <- parser
      rest <- (repeat-up-to limit parser)
      (return (cons val rest)))
     (return nil))))

(defn basic [f sz]
  (mdo
   [b i] <- get-state
   (put-state [b (+ sz i)])
   (return (f b i))))

(defn basic-n [f sz]
  (mdo
   [b i] <- get-state
   (put-state [b (+ sz i)])
   (return (f b i sz))))

(def ubyte (basic read-ubyte 1))
(def sbyte (basic read-sbyte 1))
(def uint2 (basic read-uint2 2))
(def int2 (basic read-int2 2))
(def uint4 (basic read-uint4 4))
(def int4 (basic read-int4 4))
(def utf8 (partial basic-n read-utf8))
(def binary (partial basic-n read-bytes))

(def prefixed-utf8 (>>= uint2 utf8))
(def prefixed-ascii (>>= ubyte utf8))

(defn xbytes! [arr mask]
  (amap ^bytes arr i _ (unchecked-byte (bit-xor mask (aget ^bytes arr i)))))

(defn xor-bytes [n mask]
  (basic-n
   (fn [b i n] (xbytes! (read-bytes b i n) mask))
   n))

(def prefixed-xor-ascii
  (mdo
   n <- ubyte
   a <- (binary n)
   (return (String. ^bytes (xbytes! a 0xff) "ascii"))))

(defn record [& args]
  (let [[keywords parsers] (map #(take-nth 2 %) [args (rest args)])]
    (>>= (sequence-m parsers) #(return (zipmap keywords %)))))

(defn times [n parser]
  (sequence-m (repeat n parser)))

(defn parse-until [p parser]
  (mdo
   val <- parser
   more <- (mwhen (not (p val)) (parse-until p parser))
   (return (cons val more))))

(defn tagged-parser [kw]
  (cond
   (= kw :ubyte) ubyte
   (= kw :sbyte) sbyte
   (= kw :uint2) uint2
   (= kw :int2) int2
   (= kw :uint4) uint4
   (= kw :int4) int4
   (= kw :pref-utf8) prefixed-utf8
   (= kw :nil) (return nil)
   ;; allow fallback to an actual parser
   :else (or kw (return nil))))

(def data-holder 
  (mdo
   typeid <- ubyte
   val <- (within 4 (tagged-parser (:encoding (prim/primitive typeid))))
   (return (prim/typed-value typeid val))))

(def lst
  (>>= uint2 #(times % data-holder)))

(def signature
  (mdo
   sig <- (utf8 11)
   ver <- uint2
   (skip 32)
   timestamp <- (utf8 24)
   (return [ver timestamp sig])))

(def block-header
  (record :id (utf8 4)
          :size uint4
          :flags uint2))

(defmulti block-body :id)

(defmethod block-body "ENTP" [_]
  (trace "Parsing ENTP")
  (record :entry-point-offset uint4
          :method-header-size uint2
          :exception-table-entry-size uint2
          :debugger-line-table-entry-size uint2
          :debug-table-header-size uint2
          :debug-table-local-symbol-record-header-size uint2
          :debug-records-version-number uint2))

;; symbol table
(defmethod block-body "SYMD" [_]
  (trace "Parsing SYMD")
  (mdo
   n <- uint2
   entries <- (times n (record :value data-holder :name prefixed-ascii))
   (return {:entry-count n :entries entries})))

;; function set references
(defmethod block-body "FNSD" [_]
  (trace "Parsing FNSD")
  (mdo
   n <- uint2
   entries <- (times n prefixed-ascii)
   (return {:entry-count n :entries entries})))

;; code page definition
(defmethod block-body "CPDF" [_]
  (trace "Parsing CPDF")
  (record :pool-id uint2
          :page-count uint4
          :page-size uint4))

;; code page
(defmethod block-body "CPPG" [{size :size}]
  (trace "Parsing CPPG")
  (mdo
   m <- (record :pool-id uint2
                :pool-index uint4)
   mask <- ubyte
   bytes <- (xor-bytes (- size 7) mask)
   (return (assoc m :bytes bytes))))

;; metaclass references
(defmethod block-body "MCLD" [_]
  (trace "Parsing MCLD")
  (mdo
   n <- uint2
   entries <- (times n (record :offset uint2
                               :name prefixed-ascii
                               :pids (mdo
                                      pid-count <- uint2
                                      pid-size <- uint2
                                      (times pid-count uint2))))
   (return {:entry-count n :entries entries})))

;; object block
(defmethod block-body "OBJS" [_]
  (trace "Parsing OBJS")
  (mdo
   n <- uint2
   mcld-index <- uint2
   flags <- uint2
   let large = (= (bit-and flags 1) 1)
   objects <- (times n (mdo
                        oid <- uint4
                        count <- (if large uint4 uint2)
                        bytes <- (binary count)
                        (return {:oid oid :bytes bytes})))
   (return {:mcld-index mcld-index
            :large large
            :transient (= (bit-and flags 2) 1)
            :objects objects})))

(defn binary-at [idx size]
  (>> (skip-to idx) (binary size)))

;; resources
(defmethod block-body "MRES" [_]
  (trace "Parsing MRES")
  (mdo
   [b start] <- get-state
   n <- uint2
   toc <- (times n (record :offset uint4 :size uint4 :name prefixed-xor-ascii))
   let names = (map :name toc)
   entries <- (sequence-m
               (map
                (fn [{:keys [offset size]}]
                  (binary-at (+ offset start) size))
                toc))
   (return (zipmap names entries))))

;; end block
(defmethod block-body "EOF " [_]
  (trace "Parsing EOF")
  (return {}))

(def block
  "Parse an image block, merging header and payload into map."
  (mdo
   header <- block-header
   data <- (within (:size header)
                   (block-body header))
   (return (merge header data))))

(def image
  "Parse a binary image (as sequence of blocks)."
  (>> signature (parse-until #(= (:id %) "EOF ") block)))

(defn spec 
  "Parse values according to argument spec for VM operation.
   Returns ordered map."
  [args]
  (let [[types names] (map #(take-nth 2 %) [args (rest args)])]
    (mdo
     vals <- (sequence-m (map tagged-parser types))
     (return (into (array-map) (map (fn [n v] [(keyword n) v]) names vals))))))

(defn method-header
  "Parse method header (based on method header size defined for the image)."
  [size]
  (>>=
   (within size (record :param-count ubyte
                        :opt-param-count ubyte
                        :local-variable-count uint2
                        :max-slots uint2
                        :etable-offset uint2
                        :dtable-offset uint2))
   #(return (assoc % :code-offset size))))

(def exception-table
  "Parse an exception table."
  (mdo
   n <- uint2
   (times n (record :first-offset uint2
                    :last-offset uint2
                    :oid uint4
                    :handler-offset uint2))))

(defn load-image-file [f]
  (let [buf (nio/mmap f)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)]
    buf))

(defn parse-file
  "Load and parse image file."
  [name]
  (let [f (io/file name)
        buf (load-image-file f)]
    (parse image buf)))

(defn parse-resource
  "Load and parse image file resource."
  [name]
  (when-let [f (io/file (io/resource name))]
    (parse image (load-image-file f))))
