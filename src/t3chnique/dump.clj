(ns ^{:doc "Simple dump and load of VM state."}
  t3chnique.dump
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [clojure.edn :refer [read]]
            [clojure.tools.logging :refer [debug]]
            [t3chnique.metaclass :as mc]
            [t3chnique.primitive :as p]
            [t3chnique.all])
  (:import [java.util Date]
           [java.text DateFormat]
           [java.io PushbackReader]))

(defmethod print-method (type (byte-array [])) [ba ^java.io.Writer w]
  (.write w "#bytes ")
  (.write w (pr-str (into [] ba))))

(defn prune-underscore-keys
  "Prune 'transient' items identified by keys starting with underscore."
  [item]
  (if (map? item)
    (let [ukeys (->> (keys item)
                     (filter keyword?)
                     (map name)
                     (filter #(.startsWith %  "_"))
                     (map keyword))]
      (if (seq ukeys)
        (apply dissoc item ukeys)
        item))
    item))

(defn clean-state
  [s]
  (postwalk (comp prune-underscore-keys) s))

(defn dump-state
  "Dump VM state as EDN to filename (automatically crete based on datetime
   if not supplied."
  ([s filename]
     (binding [*print-level* false
               *print-length* false]
       (->> s
            (clean-state)
            (pr-str)
            (spit filename))))
  ([s]
     (let [fmt (DateFormat/getDateTimeInstance DateFormat/SHORT DateFormat/SHORT)
           now (Date.)
           filename (string/replace
                     (str "state-" (.format fmt now) ".edn")
                     #"[ :/]"
                     "_")]
       (dump-state s filename))))

(defn load-state
  "Load VM from EDN state in named file."
  [file]
  (let [readers (merge @mc/data-readers
                       {'t3chnique.primitive.TypedValue p/map->TypedValue
                        'bytes byte-array})]
    (with-open [rdr (PushbackReader. (io/reader file))]
      (read {:readers readers} rdr))))

