(ns t3chnique.intrinsics
  (:refer-clojure :exclude [min max rand concat])
  (:require [clojure.string :as string]
            [t3chnique.monad :as m]))

(defmacro def-function-set
  "Generate a protocol definition for a function set but also add
metadata to support call by index."
  [name version-string desc & fs]
  `(do
     (defprotocol ~name
       ~desc
       ~@(map (fn [f] (list f '[_ argc])) fs))

     (def ~name (with-meta ~name {:id ~(str name "/" version-string)
                                  :fns ~(vec (map
                                              (fn [f] `(var ~f))
                                              fs))}))))

(def-function-set t3vm "010006"
  "Internal VM operations"
  t3RunGC
  t3SetSay
  t3GetVMVsn
  t3GetVMID
  t3GetVMBanner
  t3GetVMPreinitMode
  t3DebugTrace
  t3GetGlobalSymbols
  t3AllocProp
  t3GetStackTrace
  t3GetNamedArg
  t3GetNamedArgList)

(def-function-set tads-gen "030008"
  "General utility and data manipulation functions"
  dataType
  getArg
  firstObj
  nextObj
  randomize
  rand
  toString
  toInteger
  getTime
  rexMatch
  rexSearch
  rexGroup
  rexReplace
  savepoint
  undo
  saveGame
  restoreGame
  restartGame
  max
  min
  makeString
  getFuncParams
  toNumber
  sprintf
  makeList
  abs
  sgn
  concat
  rexSearchLast)

(def-function-set tads-io "030007"
  "Interactive / Real Time IO"
  tadsSay
  setLogFile
  clearScreen
  morePrompt
  inputLine
  inputKey
  inputEvent
  inputDialog
  inputFile
  timeDelay
  systemInfo
  statusMode
  statusRight
  resExists
  setScriptFile
  getLocalCharSet
  flushOutput
  inputLineTimeout
  inputLineCancel
  bannerCreate
  bannerDelete
  bannerClear
  bannerSay
  bannerFlush
  bannerSizeToContents
  bannerGoTo
  bannerSetTextColor
  bannerSetScreenColor
  bannerGetInfo
  bannerSetSize
  logConsoleCreate
  logConsoleClose
  logConsoleSay
  logInputEvent)

(def-function-set tads-net "030001"
  "Access to network features"
  connectWebUI
  getNetEvent
  getHostName
  getLocalIP
  getNetStorageURL
  getLaunchHostAddr
  sendNetRequest)

(defn parse-id
  "Parse a function set id into name and version."
  [fsid]
  (update-in (string/split fsid #"/") [1] #(Integer/parseInt %)))

(defn- match
  "True if the function set identified by required can be satisfied by the function
set identified by provided."
  [required provided]
  (let [[required-name required-version] (parse-id required)
        [provided-name provided-version] (parse-id provided)]
    (and (= required-name provided-name)
         (<= required-version provided-version ))))

(defn find-protocol
  "Find protocol for the specified function set id."
  [fsid]
  (let [protocols [t3vm tads-gen tads-io tads-net]]
    (first (filter #(match fsid (:id (meta %))) protocols))))

(defn invoke-by-index
  "Assuming host implements the required function sets, invoke the
method in the function set identified by fsid at index n, passing
specified args."
  [host fsid n & args]
  {:pre [(number? n)]}
  (let [prot (or (find-protocol fsid) (m/abort (str "Missing function set " fsid)))
        f (get (:fns (meta prot)) n)]
    (apply f host args)))
