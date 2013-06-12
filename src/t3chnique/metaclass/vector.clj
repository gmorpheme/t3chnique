(ns t3chnique.metaclass.vector
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.vm :as vm]
            [t3chnique.monad :as m])
  (:use [clojure.algo.monads :only [domonad with-monad m-seq fetch-val]]
        [t3chnique.monad :only [vm-m do-vm m-apply]]
        [t3chnique.parse :only [uint2 uint4 data-holder times record byteparser-m prefixed-utf8]])
  (:import [t3chnique.metaclass MetaClass]))

(def vec-table
  [
    (fn vec-undef [])                                         ; 0 
    (fn vec-to-list [])                                       ; 1 
    (fn vec-get-size [])                                      ; 2 
    (fn vec-copy-from [])                                     ; 3 
    (fn vec-fill-val [])                                      ; 4 
    (fn vec-subset [])                                        ; 5 
    (fn vec-apply-all [])                                     ; 6 
    (fn vec-index-which [])                                   ; 7 
    (fn vec-for-each [])                                      ; 8 
    (fn vec-for-each-assoc [])                                ; 9 
    (fn vec-map-all [])                                      ; 10 
    (fn vec-index-of [])                                     ; 11 
    (fn vec-val-which [])                                    ; 12 
    (fn vec-last-index-of [])                                ; 13 
    (fn vec-last-index-which [])                             ; 14 
    (fn vec-last-val-which [])                               ; 15 
    (fn vec-count-of [])                                     ; 16 
    (fn vec-count-which [])                                  ; 17 
    (fn vec-get-unique [])                                   ; 18 
    (fn vec-append-unique [])                                ; 19 
    (fn vec-sort [])                                         ; 20 
    (fn vec-set-length [])                                   ; 21 
    (fn vec-insert-at [])                                    ; 22 
    (fn vec-remove-element-at [])                            ; 23 
    (fn vec-remove-range [])                                 ; 24 
    (fn vec-append [])                                       ; 25 
    (fn vec-prepend [])                                      ; 26 
    (fn vec-append-all [])                                   ; 27 
    (fn vec-remove-element [])                               ; 28 
    (fn vec-splice [])                                       ; 29 
    (fn vec-join [])                                         ; 30 
    (fn vec-generate [])                                     ; 31 
    (fn vec-indexOfMin [])                                   ; 32 
    (fn vec-minVal [])                                       ; 33 
    (fn vec-indexOfMax [])                                   ; 34 
    (fn vec-maxVal [])                                       ; 35 
    ])

(defrecord Vector [v]

  MetaClass

  (load-from-image [self buf o])

  ; [] [capacity] [src] [capacity src] [src copy-count]
  (load-from-stack [_ argc]

    (case argc
      0 (do-vm [] (Vector. []))
      1 (do-vm
         [arg (vm/stack-pop)]
         (if (p/vm-int? arg)
           (Vector. [])
           (m/abort "todo vector copy")))
      2 (do-vm
         [arg1 (vm/stack-pop)
          arg2 (vm/stack-pop)]
         (if (every? p/vm-int? [arg1 arg2])
           (Vector. (repeat (p/value arg2) (p/vm-nil)))
           (m/abort "todo vector copy")))
      :else (m/abort "VMERR_WRONG_NUM_OF_ARGS")))

  (get-property [self propid argc]
    (let [mcidx (:metaclass self)]
      (do-vm
       [[obj {f :value}] (m-apply #(mc/get-intrinsic-method % mcidx propid vec-table))
        r (f argc)]
       r)))
  
  (list-like? [_ _] true))

(defn tads-vector
  ([] (Vector. []))
  ([src] (Vector. src)))

(mc/register-metaclass! "vector/030005" tads-vector)