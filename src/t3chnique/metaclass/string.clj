(ns t3chnique.metaclass.string
  (:require [t3chnique.metaclass :as mc]
            [clojure.algo.monads :refer [domonad with-monad m-seq fetch-val]]
            [t3chnique.parse :refer [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]])
  (:import [t3chnique.metaclass MetaClass]))

(def fn-table
  [
    (fn undef [])                                         ; 0 TODO
    (fn len [])                                           ; 1 TODO
    (fn substr [])                                        ; 2 TODO
    (fn upper [])                                         ; 3 TODO
    (fn lower [])                                         ; 4 TODO
    (fn find [])                                          ; 5 TODO
    (fn to-uni [])                                        ; 6 TODO
    (fn htmlify [])                                       ; 7 TODO
    (fn starts-with [])                                   ; 8 TODO
    (fn ends-with [])                                     ; 9 TODO
    (fn to-byte-array [])                                ; 10 TODO
    (fn replace [])                                      ; 11 TODO
    (fn splice [])                                       ; 12 TODO
    (fn split [])                                        ; 13 TODO
    (fn specialsToHtml [])                               ; 14 TODO
    (fn specialsToText [])                               ; 15 TODO
    (fn urlEncode [])                                    ; 16 TODO
    (fn urlDecode [])                                    ; 17 TODO
    (fn sha256 [])                                       ; 18 TODO
    (fn md5 [])                                          ; 19 TODO
    (fn packBytes [])                                    ; 20 TODO
    (fn unpackBytes [])                                  ; 21 TODO
    (fn toTitleCase [])                                  ; 22 TODO
    (fn toFoldedCase [])                                 ; 23 TODO
    (fn compareTo [])                                    ; 24 TODO
    (fn compareIgnoreCase [])                            ; 25 TODO
    (fn findLast [])                                     ; 26 TODO
    (fn findAll [])                                      ; 27 TODO
    (fn match [])                                        ; 28 TODO
   ])

(defrecord TadsString [text]
  MetaClass
  (load-from-image [self buf o]
    (with-monad byteparser-m
      (TadsString. (first ((prefixed-utf8) [buf o]))))))

(defn tads-string [] (TadsString. nil))


(mc/register-metaclass! "string/030008" tads-string)