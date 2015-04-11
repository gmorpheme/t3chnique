(ns t3chnique.test-server
  (:require [t3chnique.server :as sv])
  (:use [midje.sweet]))

(future-fact "VM new"
  (let [s (sv/system)]
    (sv/vm-new! s "cube.t3")
    (count @(:vm-histories s)) => 1
    (sv/vm-destroy! s)
    (count @(:vm-histories s)) => 0))

