(ns t3chnique.metaclass.string
  (:require [t3chnique.metaclass :as mc]
            [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]
            [clojure.tools.logging :refer [trace debug]]
            [t3chnique.parse :refer [parse prefixed-utf8]]
            [monads.core :refer [mdo return]]))

(declare add-to-str)

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
  mc/MetaClass
  
  (load-from-image [self buf o]
    (TadsString. (parse prefixed-utf8 [buf o])))

  (get-as-string [self]
    (trace "get-as-string" self)
    text)

  (cast-to-string [_] (return text))

  (add [self val]
    (add-to-str (p/vm-sstring self) val)))

(defn tads-string
  "Create a TadsString."
  ([]
     (trace "create tads-string")
     (TadsString. nil))
  ([text]
     (trace "create tads-string(" text ")")
     (TadsString. text)))

(mc/register-metaclass! "string/030008" tads-string)
(mc/register-data-reader! 't3chnique.metaclass.string.TadsString map->TadsString)

(defn create
  "Action to create string "
  [text]
  (vm/obj-intern (tads-string text)))

(defn- shortcut-add
  "Potentially we can shortcut a string addition if one is empty."
  [left left-text right right-text]
  (cond
   (or (p/vm-nil? right) (empty? right-text)) left
   (and (empty? left) (vm/as-string right)) right
   :else nil))

(defn add-to-str
  "Concatenate left and right into a newly allocated string."
  [left right]
  (trace "add-to-str" left right)
  (mdo
   left-text <- (vm/as-string left)
   right-text <- (vm/convert-to-string right)
   (if-let [val (shortcut-add left left-text right right-text)]
     (return val)
     (create (str left-text right-text)))))
