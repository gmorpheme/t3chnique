(ns t3chnique.parse
  (:use [clojure.algo.monads])
  (:import [java.nio.charset Charset]
           [java.nio ByteOrder MappedByteBuffer ByteBuffer]))

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

(defprotocol ByteSource
  (read-sbyte [self idx])
  (read-ubyte [self idx])
  (read-bytes [self idx count])
  (read-int2 [self idx])
  (read-uint2 [self idx])
  (read-int4 [self idx])
  (read-uint4 [self idx])
  (read-utf8 [self idx count]))

(extend-protocol ByteSource
  ByteBuffer
  (read-sbyte [b idx] (.get b))
  (read-ubyte [b idx] (bit-and (.get b) 0xff))
  (read-bytes [b idx count] (let [dest (byte-array count)]
                              (.get b dest 0 count)
                              dest))
  (read-int2 [b idx] (.getShort b))
  (read-uint2 [b idx] (bit-and (.getShort b) 0xffff))
  (read-int4 [b idx] (.getInt b))
  (read-uint4 [b idx] (bit-and (.getInt 0xffffffff)))
  (read-utf8 [b idx count] (let [utf8 (Charset/forName "utf-8")
                                 decoder (.newDecoder utf8)
                                 slice (slice b count)]
                             (String. (.array (.decode decoder slice))))))


(def byteparser-m (state-t maybe-m))


(defmacro defprimitive [n f s]
  `(defn ~n []
     (domonad byteparser-m
      [[b# i#] (fetch-val :parse-state)
       _# (set-val :parse-state [b# (+ ~s i#)])]
      (~f b# i#))))

(with-monad byteparser-m
  
  (defprimitive ubyte read-ubyte 1)
  (defprimitive sbyte read-sbyte 1)
  (defprimitive uint2 read-uint2 2)
  (defprimitive int2 read-int2 2)
  (defprimitive uint4 read-uint4 4)
  (defprimitive int4 read-int4 4)

  (defn utf8 [byte-count]
    (domonad
     [[b i] (fetch-val :parse-state)
      _ (set-val :parse-state [b (+ byte-count i)])]
     (read-utf8 b i byte-count)))

  (defn prefixed-utf8 []
    (domonad
     [count (ubyte)
      value (utf8 count)]
     value))

  (defn record [& args]
    (let [[keywords parsers] (map #(take-nth 2 %) [args (rest args)])]
      (domonad
       [vals (m-seq parsers)]
       (zipmap keywords vals))))

  (defn times [n parser]
    (m-seq (repeat n parser)))


  )

;; testing functions

(defn init [buf] {:parse-state [buf 0]})

(defn make-buf [bytes]
  (ByteBuffer/wrap
   (byte-array
    (map unchecked-byte bytes))))

(defn utf8-buf [str]
  (make-buf (seq (.getBytes str "utf-8"))))

(defn utf8-pbuf [c str]
  (make-buf (cons c (seq (.getBytes str "utf-8")))))
