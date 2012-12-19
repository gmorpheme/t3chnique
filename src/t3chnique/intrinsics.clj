(ns t3chnique.intrinsics
  (:refer-clojure :exclude [_ argc]))

(defprotocol ^{:id "t3vm/010006"} t3vm
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

(defprotocol ^{:id "tads-gen/030008"} tads-gen
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

(defprotocol ^{:id "tads-io/030007"} tads-io
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

(defprotocol ^{:id "tads-net/030001"} tads-net
  "Access to network features."
  (connectWebUI [_ argc])
  (getHostName [_ argc])
  (getLaunchHostAddr [_ argc])
  (getLocalIP [_ argc])
  (getNetEvent [_ argc])
  (getNetStorageURL [_ argc])
  (sendNetRequest [_ argc]))
