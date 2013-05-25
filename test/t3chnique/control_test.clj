(ns t3chnique.control-test
  (:use [midje.sweet]
        [t3chnique.control]))

(let [vm (vm-new 1)
      id (:id vm)]
  (facts "Test that only enter is available on an unitialised vm and step thereafter"
    (println id)
    (vm-actions vm) => [:action/enter]
    (vm-enter id)
    (vm-actions (vm-get id)) => [:action/step])
  (vm-destroy! id))