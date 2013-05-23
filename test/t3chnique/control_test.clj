(ns t3chnique.control-test
  (:use [midje.sweet]
        [t3chnique.control]))

(fact
  (vm-actions (vm-new 1)) => [:action/enter])
