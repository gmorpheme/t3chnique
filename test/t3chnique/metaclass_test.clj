(ns t3chnique.metaclass_test
  (:use [midje.sweet]
        [t3chnique.metaclass])
  (:require [t3chnique.vm :as vm]
            [t3chnique.primitive :as p]))


(fact
  (let [buf (byte-array
             (map unchecked-byte '(0 0 8 0 1 0 1 0 11 -94 4 0 0 54 0 1 0 0 0 0 66 0 1 0 0 0 0 -90 0 1 0 0 0 0 -74 4 1 0 0 0 0 -66 4 10 -121 1 0 0 120 5 11 -52 4 0 0 82 6 1 0 0 0 0)))]
    (load-from-image (tads-object) buf 0) => {:bases []
                                              :is-class true
                                              :properties {1 (p/vm-codeofs 1186)
                                                           54 (p/vm-nil)
                                                           66 (p/vm-nil)
                                                           166 (p/vm-nil)
                                                           1206 (p/vm-nil)
                                                           1214 (p/vm-list 391)
                                                           1400 (p/vm-codeofs 1228)
                                                           1618 (p/vm-nil)}}))