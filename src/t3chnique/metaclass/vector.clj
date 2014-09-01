(ns t3chnique.metaclass.vector
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.metaclass.object :as obj]
            [t3chnique.vm :as vm]
            [t3chnique.metaclass.collection :as coll]
            [t3chnique.metaclass.object :as obj]
            [clojure.tools.logging :refer [trace]]
            [monads.core :refer [mdo return >>=]]))

(defmacro abort [& args]
  `(throw (ex-info ~@args)))

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

  mc/MetaClass

  ;; action: first arg is capacity (ignored), then source object or initial count
  (load-from-stack [_ argc]
    (case argc
      0 (return (Vector. []))
      1 (>>= vm/stack-pop #(if (p/vm-int? %) (return (Vector. [])) (abort "TODO: vector copy")))
      2 (mdo
         arg1 <- vm/stack-pop
         arg2 <- vm/stack-pop
         (if (every? p/vm-int? [arg1 arg2])
           (return (Vector. (repeat (p/value arg2) (p/vm-nil))))
           (abort "TODO: vector copy")))
      :else (abort "VMERR_WRONG_NUM_OF_ARGS")))

  (load-from-image [self buf o]
    )                                   ; TODO: load vector from image
  
  ;; action: lookup and invoke intrinsic (returning [intcls action]) 
  (get-property [self propid argc]
    (mc/default-get-property
      self
      propid
      argc
      :vector vec-table
      :collection coll/property-table
      :object obj/property-table))

  (list-like? [_ _] true)

  (get-as-string [_] nil))

(defn tads-vector
  ([]
     (trace "create tads-vector")
     (Vector. []))
  ([src]
     (trace "create tads-vector(" src ")")
     (Vector. src)))

(mc/register-metaclass! "vector/030005" tads-vector)
(mc/register-data-reader! 't3chnique.metaclass.vector.Vector map->Vector)
