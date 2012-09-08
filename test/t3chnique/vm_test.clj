(ns t3chnique.vm-test
  (:use clojure.test
        t3chnique.vm))

(deftest test-add
  (testing "Integer addition"
    (doseq [i (range 10) j (range 10)]
      (vm-reset)
      (pushint8 i)
      (pushint8 j)
      (add)
      (is (= (stack-peek) (vm-int (+ i j)))))))

(deftest test-eq
  (testing "Integer comparison"
    (doseq [i (range 10) j (range 10)]
      (vm-reset)
      (pushint i)
      (pushint j)
      (eq)
      (is (= (vm-bool (= i j)) (stack-peek))))))

(deftest test-neq
  (testing "Integer comparison"
    (doseq [i (range 10) j (range 10)]
      (vm-reset)
      (pushint i)
      (pushint j)
      (ne)
      (is (= (vm-bool (not= i j)) (stack-peek))))))

