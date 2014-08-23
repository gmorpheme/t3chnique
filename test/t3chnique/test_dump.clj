(ns t3chnique.test-dump
  (:require [t3chnique.dump :as dump])
  (:use [midje.sweet]))

(fact "prune-underscore-keys prunes"
  (dump/prune-underscore-keys {:_a 1 :a 2}) => {:a 2}
  (dump/prune-underscore-keys {:_a 1}) => {})

(fact "prune-underscore-keys only changes maps"
  (dump/prune-underscore-keys [1 2 3]) => [1 2 3]
  (dump/prune-underscore-keys "blah") => "blah")
