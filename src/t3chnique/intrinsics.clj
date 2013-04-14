(ns t3chnique.intrinsics
  (:refer-clojure :exclude [min max rand concat])
  (:require [clojure.string :as string]))

(defprotocol t3vm
  "Internal VM operations"
  (t3RunGC [_ argc])
  (t3SetSay [_ argc])
  (t3GetVMVsn [_ argc])
  (t3GetVMID [_ argc])
  (t3GetVMBanner [_ argc])
  (t3GetVMPreinitMode [_ argc])
  (t3DebugTrace [_ argc])
  (t3GetGlobalSymbols [_ argc])
  (t3AllocProp [_ argc])
  (t3GetStackTrace [_ argc])
  (t3GetNamedArg [_ argc])
  (t3GetNamedArgList [_ argc]))

(def t3vm (with-meta t3vm {:id "t3vm/010006"
                           :fns [t3RunGC
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
                                 t3GetNamedArgList]}))

(defprotocol tads-gen
  "General utility and data manipulation functions"
  (abs [_ argc])
  (concat [_ argc])
  (dataType [_ argc])
  (firstObj [_ argc])
  (getArg [_ argc])
  (getFuncParams [_ argc])
  (getTime [_ argc])
  (makeList [_ argc])
  (makeString [_ argc])
  (max [_ argc])
  (min [_ argc])
  (nextObj [_ argc])
  (rand [_ argc])
  (randomize [_ argc])
  (restartGame [_ argc])
  (restoreGame [_ argc])
  (rexGroup [_ argc])
  (rexMatch [_ argc])
  (rexReplace [_ argc])
  (rexSearch [_ argc])
  (rexSearchLast [_ argc])
  (saveGame [_ argc])
  (savepoint [_ argc])
  (sgn [_ argc])
  (sprintf [_ argc])
  (toInteger [_ argc])
  (toNumber [_ argc])
  (toString [_ argc])
  (undo [_ argc]))

(def tads-gen (with-meta tads-gen {:id "tads-gen/030008"}))

(defprotocol tads-io
  "Interactive / Real Time IO"
  (bannerClear [_ argc])
  (bannerCreate [_ argc])
  (bannerDelete [_ argc])
  (bannerFlush [_ argc])
  (bannerGetInfo [_ argc])
  (bannerGoTo [_ argc])
  (bannerSay [_ argc])
  (bannerSetScreenColor [_ argc])
  (bannerSetSize [_ argc])
  (bannerSetTextColor [_ argc])
  (bannerSizeToContents [_ argc])
  (clearScreen [_ argc])
  (flushOutput [_ argc])
  (getLocalCharSet [_ argc])
  (inputDialog [_ argc])
  (inputEvent [_ argc])
  (inputFile [_ argc])
  (inputKey [_ argc])
  (inputLine [_ argc])
  (inputLineCancel [_ argc])
  (inputLineTimeout [_ argc])
  (logConsoleClose [_ argc])
  (logConsoleCreate [_ argc])
  (logConsoleSay [_ argc])
  (morePrompt [_ argc])
  (resExists [_ argc])
  (setLogFile [_ argc])
  (setScriptFile [_ argc])
  (statusMode [_ argc])
  (statusRight [_ argc])
  (systemInfo [_ argc])
  (tadsSay [_ argc])
  (timeDelay [_ argc]))

(def tads-io (with-meta tads-io {:id "tads-io/030007"}))

(defprotocol tads-net
  "Access to network features."
  (connectWebUI [_ argc])
  (getHostName [_ argc])
  (getLaunchHostAddr [_ argc])
  (getLocalIP [_ argc])
  (getNetEvent [_ argc])
  (getNetStorageURL [_ argc])
  (sendNetRequest [_ argc]))

(def tads-net (with-meta tads-net {:id "tads-net/030001"}))

(defn- parse-id
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
         (< required-version provided-version ))))

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
  (let [prot (find-protocol fsid)
        f (get (:fns (meta prot)) n)]
    (apply f host args)))