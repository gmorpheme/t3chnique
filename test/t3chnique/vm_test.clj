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
    (are [ops stack] (= (apply stack-after ops) stack)
         [(op-push_0)] [(vm-int 0)]
         [(op-push_1)] [(vm-int 1)]
         [(op-pushint8 100)] [(vm-int 100)]
         [(op-pushint 123456)] [(vm-int 123456)]
         [(op-pushlst 0xff)] [(vm-list 0xff)]
         [(op-pushnil) (op-pushtrue)] [(vm-nil) (vm-true)]
         [(op-pushenum 9876)] [(vm-enum 9876)])))

(deftest test-arithmetic
  (testing "Testing inc"
    (doseq [i (range 100)]
      (are [ops stack] (= (apply stack-after ops) stack)
           [(op-pushint i) (op-inc)] [(vm-int (inc i))])))
  (testing "Testing dec"
    (doseq [i (range 100)]
      (are [ops stack] (= (apply stack-after ops) stack)
           [(op-pushint i) (op-dec)] [(vm-int (dec i))])))
  (testing "Integer addition"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-add)) [(vm-int (+ i j))]))))
  (testing "Integer equality"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-eq)) [(vm-bool (= i j))]))))
  (testing "Integer inequality"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-ne)) [(vm-bool (not= i j))]))))
  (testing "Integer subtraction"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-sub)) [(vm-int (- i j))]))))
  (testing "Integer multiplication"
    (doseq [i (range 10) j (range 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-mul)) [(vm-int (* i j))]))))
  (testing "Integer division"
    (doseq [i (range 10) j (range 1 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-div)) [(vm-int (/ i j))]))))
  (testing "Integer modulo"
    (doseq [i (range 10) j (range 1 10)]
      (is (= (stack-after (op-pushint8 i) (op-pushint8 j) (op-mod)) [(vm-int (mod i j))]))))  
  (testing "Negation"
    (doseq [i (range 100)]
      (is (= (stack-after (op-pushint8 i) (op-neg)) [(vm-int (- i))])))))

(deftest test-logical
  (testing "Not"
    (are [ops stack] (= (apply stack-after ops) stack)
         [(op-pushnil) (op-not)] [(vm-true)]
         [(op-pushtrue) (op-not)] [(vm-nil)]
         [(op-pushint8 0) (op-not)] [(vm-true)]
         [(op-pushint8 1) (op-not)] [(vm-nil)]
         [(op-pushint 0) (op-not)] [(vm-true)]
         [(op-pushint 1) (op-not)] [(vm-nil)]))
  (testing "Boolize"
    (are [ops stack] (= (apply stack-after ops) stack)
         [(op-pushnil) (op-boolize)] [(vm-nil)]
         [(op-pushtrue) (op-boolize)] [(vm-true)]
         [(op-pushint8 0) (op-not)] [(vm-true)]
         [(op-pushint8 1) (op-not)] [(vm-nil)]
         [(op-pushint 0) (op-not)] [(vm-true)]
         [(op-pushint 1) (op-not)] [(vm-nil)])))

(deftest test-bitwise
  (testing "bnot"
    #_(is (= (stack-after (op-pushint 0xffffffff) (op-bnot)) [(vm-int 0x00000000)]))))