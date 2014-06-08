(ns t3chnique.metaclass-test
  (:use [midje.sweet]
        [t3chnique.metaclass]
        [t3chnique.primitive :as p]
        [t3chnique.monad :as m])
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
               :pids [2 4 6 8 10]}
              {:metaclass-id :bar
               :pids [3 5 7 9 11]}]
        foo-table ['a 'b 'c 'd 'e]
        bar-table ['A 'B 'C 'D 'E]]
    (p/value (lookup-intrinsic {:mcld mcld} 2 :foo foo-table :bar bar-table)) => 'a
    (p/value (lookup-intrinsic {:mcld mcld} 3 :foo foo-table :bar bar-table)) => 'A
    (p/value (lookup-intrinsic {:mcld mcld} 4 :foo foo-table :bar bar-table)) => 'b
    (p/value (lookup-intrinsic {:mcld mcld} 5 :foo foo-table :bar bar-table)) => 'B
    (p/value (lookup-intrinsic {:mcld mcld} 6 :foo foo-table :bar bar-table)) => 'c
    (p/value (lookup-intrinsic {:mcld mcld} 7 :foo foo-table :bar bar-table)) => 'C

    (p/value (m/eval-vm (lookup-intrinsic-m 8 :foo foo-table) {:mcld mcld})) => 'd
    (p/value (m/eval-vm (lookup-intrinsic-m 9 :bar bar-table) {:mcld mcld})) => 'D))



