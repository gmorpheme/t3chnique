(ns t3chnique.vm-test
  (:use clojure.test
        clojure.algo.monads
        t3chnique.vm
        t3chnique.primitive))

(defn stack-after [& ops]
  (with-monad state-m
    (:stack (second ((m-seq ops) (vm-state))))))

(deftest test-pushes
  (testing "Simple pushes"
    (is (= (stack-after (op-push_0)) [(vm-int 0)]))
    (is (= (stack-after (op-push_1)) [(vm-int 1)]))
    (is (= (stack-after (op-pushint8 100)) [(vm-int 100)]))
    (is (= (stack-after (op-pushint 123456)) [(vm-int 123456)]))
    (is (= (stack-after (op-pushlst 0xff)) [(vm-list 0xff)]))
    (is (= (stack-after (op-pushnil) (op-pushtrue)) [(vm-nil) (vm-true)]))))

(deftest test-add
  (testing "Integer addition"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-add)) [(vm-int (+ i j))])))))
