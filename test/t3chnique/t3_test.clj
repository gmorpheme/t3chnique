(ns ^{:doc "Run compiled t3 test cases from TADS3 source"}
  t3chnique.t3-test
  (:require [t3chnique.vm :as vm]
            [t3chnique.parse :as parse])
  (:use [midje.sweet]))

(defn load-and-run [name]
  (let [m (vm/vm-from-image (parse/parse-resource name))]
    (let [[r m0] ((vm/enter) m)]
      ((vm/run-state) m0))))

(future-fact
  (first (load-and-run "vector.t3")) => nil?)

