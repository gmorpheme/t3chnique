(ns t3chnique.metaclass-test
  (:use [midje.sweet]
        [t3chnique.metaclass]
        [t3chnique.primitive :as p])
  (:require [t3chnique.vm :as vm]))


(facts "MCLD lookups"
  (find-metaclass-by-id [{:metaclass-id :foo :tag "foo"}]
                        :foo)
  => (contains {:tag "foo"})

  (find-metaclass-by-id [{:metaclass-id :foo :tag "foo"}
                         {:metaclass-id :bar :tag "bar"}]
                        :foo)
  => (contains {:tag "foo"})

  (find-metaclass-by-id [{:metaclass-id :bar :tag "bar"}
                         {:metaclass-id :foo :tag "foo"}]
                        :foo)
  => (contains {:tag "foo"}))

(facts "Lookup of intrinsic methods"
  (let [mcld [{:metaclass-id :foo
               :intrinsic-class-oid 10
               :pids [2 4 6 8 10]}
              {:metaclass-id :bar
               :intrinsic-class-oid 20
               :pids [3 5 7 9 11]}]
        foo-table [nil 'a 'b 'c 'd 'e]
        bar-table [nil 'A 'B 'C 'D 'E]]
    (lookup-intrinsic {:mcld mcld} 2 :foo foo-table :bar bar-table) => [(p/vm-obj 10) (p/vm-native-code 'a)]
    (lookup-intrinsic {:mcld mcld} 3 :foo foo-table :bar bar-table) => [(p/vm-obj 20) (p/vm-native-code 'A)]
    (lookup-intrinsic {:mcld mcld} 4 :foo foo-table :bar bar-table) => [(p/vm-obj 10) (p/vm-native-code 'b)]
    (lookup-intrinsic {:mcld mcld} 5 :foo foo-table :bar bar-table) => [(p/vm-obj 20) (p/vm-native-code 'B)]
    (lookup-intrinsic {:mcld mcld} 6 :foo foo-table :bar bar-table) => [(p/vm-obj 10) (p/vm-native-code 'c)]
    (lookup-intrinsic {:mcld mcld} 7 :foo foo-table :bar bar-table) => [(p/vm-obj 20) (p/vm-native-code 'C)]))

(facts "Wiring up intrinsic classes"
  (let [mcld [{:metaclass-id :a}
              {:metaclass-id :b}
              {:metaclass-id :c}]
        objs {111 {:oid 111
                   :metaclass-index 0}
              999 {:oid 999
                   :metaclass-index 1}
              222 {:oid 222}}]
    (process-intrinsic-class-objects mcld objs) => [{:metaclass-id :a
                                                     :intrinsic-class-oid 111}
                                                    {:metaclass-id :b
                                                     :intrinsic-class-oid 999}
                                                    {:metaclass-id :c}]))
