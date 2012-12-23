(ns t3chnique.vm-test
  (:use [clojure.test]
        [clojure.algo.monads]
        [t3chnique.vm]
        [t3chnique.primitive]
        [midje.sweet]))

;; helpers

(defn st [& xs]
  (vec (map #(cond
              (number? %) (vm-int %)
              (string? %) (vm-sstring %)
              (= % true) (vm-true)
              :else (vm-nil))
            xs)))

(defn stack-after [& ops]
  (with-monad state-m
    (:stack (second ((m-seq ops) (vm-state))))))

(defn apply-ops [init ops]
  (with-monad state-m
    (second ((m-seq ops) init))))

(defn apply-with-stack [stack ops]
  (:stack (apply-ops (merge (vm-state) {:stack stack :sp (count stack)}) ops)))

(defn vm-state-with [& args]
  (apply assoc (vm-state) args))

;; facts

(tabular
 (fact "Simple pushes" (apply stack-after ?ops) => ?stack)
 
 ?ops                         ?stack
 [(op-push_0)]                (st 0)
 [(op-push_1)]                (st 1)
 [(op-pushint8 100)]          (st 100)
 [(op-pushint 123456)]        (st 123456)
 [(op-pushlst 0xff)]          [(vm-list 0xff)]
 [(op-pushnil) (op-pushtrue)] (st nil true)
 [(op-pushenum 9876)]         [(vm-enum 9876)])

(tabular
 (fact "Simple stack ops" (apply-with-stack ?before ?ops) => ?after)

 ?before        ?ops             ?after
 (st 10)        [(op-dup)]       (st 10 10)
 (st nil)       [(op-dup)]       (st nil nil)
 (st true)      [(op-disc)]      [] 
 (st 100 99 98) [(op-disc)]      (st 100 99)
 (st nil nil)   [(op-disc1 2)]   []
 (st nil nil)   [(op-disc1 1)]   (st nil)
 (st 1 2)       [(op-swap)]      (st 2 1)
 (st 1 2)       [(op-dup2)]      (st 1 2 1 2))

(facts "Arithmetic"
  
  (fact "Integer increment"
    (doseq [i (range 100)]
      (stack-after (op-pushint i) (op-inc)) => [(vm-int (inc i))]))

  (fact "Integer decrement"
    (doseq [i (range 100)]
      (stack-after (op-pushint i) (op-dec)) => [(vm-int (dec i))]))

  (fact "Integer addition"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-add)) => [(vm-int (+ i j))]))

  (fact "Integer equality"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-eq)) => [(vm-bool (= i j))]))

  (fact "Integer inequality"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-ne)) => [(vm-bool (not= i j))]))

  (fact "Integer subtraction"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-sub)) =>  [(vm-int (- i j))]))

  (fact "Integer multiplication"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-mul)) => [(vm-int (* i j))]))

  (fact "Integer division"
    (doseq [i (range 10) j (range 1 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-div)) => [(vm-int (/ i j))]))  

  (fact "Integer modulo"
    (doseq [i (range 10) j (range 1 10)]
      (stack-after (op-pushint8 i) (op-pushint8 j) (op-mod)) => [(vm-int (mod i j))]))

  (fact "Negation"
    (doseq [i (range 100)]
      (stack-after (op-pushint8 i) (op-neg)) => [(vm-int (- i))])))


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
    ;; TODO fix bnot
    #_(is (= (stack-after (op-pushint 0xffffffff) (op-bnot)) [(vm-int 0x00000000)]))))

(deftest test-call-stack
  (with-redefs [get-method-header (fn [ptr] (fn [s] [{:param-count 2 :opt-param-count 0 :local-variable-count 4 :code-offset 10} s]))]
    (let [st (vm-state-with :ep 0x10 :ip 0x30 :fp 0 :sp 2 :stack [(vm-int 1) (vm-int 2)])]
      (testing "call stack"
        (is (=
             (apply-ops st [(op-call 2 0x1234)])
             (assoc st
               :ep 0x1234
               :ip 0x123e
               :fp 10
               :sp 14
               :stack [(vm-int 1) (vm-int 2)
                       (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                       (vm-codeofs 0x20)
                       (vm-codeofs 0x10)
                       (vm-int 2)
                       (vm-int 0)
                       (vm-nil) (vm-nil) (vm-nil) (vm-nil)])))))))

(deftest test-returns
  (testing "Testing returns"
    (is (=
         (apply-ops
           (merge (vm-state) {:ep 0x1234
                              :ip 0x123e
                              :fp 10
                              :sp 15
                              :stack [(vm-int 1) (vm-int 2)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                      (vm-codeofs 0x20)
                                      (vm-codeofs 0x10)
                                      (vm-int 2)
                                      (vm-int 0)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                      (vm-int 99)]})
           [(op-retval)])
         (merge (vm-state) {:ep 0x20
                            :ip 0x30
                            :fp 0
                            :sp 0
                            :r0 (vm-int 99)
                            :stack []})))
    (is (=
         (apply-ops
           (merge (vm-state) {:ep 0x1234
                              :ip 0x123e
                              :fp 10
                              :sp 14
                              :stack [(vm-int 1) (vm-int 2)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                      (vm-codeofs 0x20)
                                      (vm-codeofs 0x10)
                                      (vm-int 2)
                                      (vm-int 0)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)]})
           [(op-retnil)])
         (merge (vm-state) {:ep 0x20
                            :ip 0x30
                            :fp 0
                            :sp 0
                            :r0 (vm-nil)
                            :stack []})))
    (is (=
         (apply-ops
           (merge (vm-state) {:ep 0x1234
                              :ip 0x123e
                              :fp 10
                              :sp 14
                              :stack [(vm-int 1) (vm-int 2)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                      (vm-codeofs 0x20)
                                      (vm-codeofs 0x10)
                                      (vm-int 2)
                                      (vm-int 0)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)]})
           [(op-rettrue)])
         (merge (vm-state) {:ep 0x20
                            :ip 0x30
                            :fp 0
                            :sp 0
                            :r0 (vm-true)
                            :stack []})))
    (is (=
         (apply-ops
           (merge (vm-state) {:ep 0x1234
                              :ip 0x123e
                              :fp 10
                              :sp 14
                              :r0 (vm-int 111)
                              :stack [(vm-int 1) (vm-int 2)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                      (vm-codeofs 0x20)
                                      (vm-codeofs 0x10)
                                      (vm-int 2)
                                      (vm-int 0)
                                      (vm-nil) (vm-nil) (vm-nil) (vm-nil)]})
           [(op-ret)])
         (merge (vm-state) {:ep 0x20
                            :ip 0x30
                            :fp 0
                            :sp 0
                            :r0 (vm-int 111)
                            :stack []})))))

(deftest test-local-access
  (testing "Test local access"
    (let [stack (map vm-int (range 4))]
      (doseq [i (range 4) f [op-getlcl1 op-getlcl2]]
        (is (= (apply-with-stack stack [(f i)])
               (conj stack (vm-int i))))))))

(deftest test-arg-access
  (testing "Test argument access"
    (let [init (merge (vm-state) {:fp 10
                                  :sp 14
                                  :stack [(vm-int 101) (vm-int 100)
                                          (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                          (vm-codeofs 0x20)
                                          (vm-codeofs 0x10)
                                          (vm-int 2)
                                          (vm-int 0)
                                          (vm-nil) (vm-nil) (vm-nil) (vm-nil)]})]
      (doseq [i (range 2) op [op-getarg1 op-getarg2]]
        (let [after (apply-ops init [(op i)])]
          (is (= (:sp after) 15))
          (is (= (value (last (:stack after))) (+ 100 i))))))))

(deftest test-push-self
  (testing "Test push self"
    (let [init (merge (vm-state) {:fp 10
                                  :sp 14
                                  :stack [(vm-int 101) (vm-int 100)
                                          (vm-prop 10) (vm-obj 0x12) (vm-obj 0x22) (vm-obj 0x33)
                                          (vm-codeofs 0x20)
                                          (vm-codeofs 0x10)
                                          (vm-int 2)
                                          (vm-int 0)
                                          (vm-nil) (vm-nil) (vm-nil) (vm-nil)]})
          after (apply-ops init [(op-pushself)])]
      (is (= (value (last (:stack after))) 0x33)))))

(deftest test-jump
  (testing "Jumps"
    (is (= (apply-ops (vm-state-with :ip 0x66) [(op-jmp 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 2 :stack [(vm-int 0) (vm-int 0)]) [(op-je 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 2 :stack [(vm-int 0) (vm-int 0)]) [(op-jne 0x11)])
           (vm-state-with :ip 0x66)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-true)]) [(op-jt 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-nil)]) [(op-jt 0x11)])
           (vm-state-with :ip 0x66)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-true)]) [(op-jf 0x11)])
           (vm-state-with :ip 0x66)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-nil)]) [(op-jf 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-nil)]) [(op-jnil 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-nil)]) [(op-jnotnil 0x11)])
           (vm-state-with :ip 0x66)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-true)]) [(op-jnotnil 0x11)])
           (vm-state-with :ip 0x75)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :sp 1 :stack [(vm-true)]) [(op-jnil 0x11)])
           (vm-state-with :ip 0x66)))
    (is (= (apply-ops (vm-state-with :ip 0x66 :r0 (vm-true)) [(op-jr0t 0x11)])
           (vm-state-with :ip 0x75 :r0 (vm-true))))
    (is (= (apply-ops (vm-state-with :ip 0x66 :r0 (vm-true)) [(op-jr0f 0x11)])
           (vm-state-with :ip 0x66 :r0 (vm-true))))
    (is (= (apply-ops (vm-state-with :ip 0x66 :r0 (vm-nil)) [(op-jr0f 0x11)])
           (vm-state-with :ip 0x75 :r0 (vm-nil))))
    (is (= (apply-ops (vm-state-with :ip 0x66 :r0 (vm-nil)) [(op-jr0t 0x11)])
           (vm-state-with :ip 0x66 :r0 (vm-nil))))))

(deftest test-jump-and-save
  (let [false-state (vm-state-with :ip 0x66 :sp 2 :stack [(vm-int 0) (vm-int 0)])
        true-state (vm-state-with :ip 0x66 :sp 2 :stack [(vm-int 0) (vm-int 1)])]
    (testing "Jump and save"
      (is (= (apply-ops false-state [(op-jst 0x11)])
             (assoc false-state :sp 1 :stack [(vm-int 0)])))
      (is (= (apply-ops true-state [(op-jst 0x11)])
             (assoc true-state :ip 0x75)))
      (is (= (apply-ops false-state [(op-jsf 0x11)])
             (assoc false-state :ip 0x75)))
      (is (= (apply-ops true-state [(op-jsf 0x11)])
             (assoc true-state :sp 1 :stack [(vm-int 0)]))))))

(deftest test-stack-access
  (let [st (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 10
                          :sp 14
                          :stack [(vm-int 1) (vm-int 2)
                                  (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                  (vm-codeofs 0x20)
                                  (vm-codeofs 0x10)
                                  (vm-int 2)
                                  (vm-int 0)
                                  (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                                  (vm-int 999)])]
    (is (= (apply-ops st [(op-setarg1 0)])
           (assoc st
             :sp 13
             :stack [(vm-int 1) (vm-int 999)
                     (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                     (vm-codeofs 0x20)
                     (vm-codeofs 0x10)
                     (vm-int 2)
                     (vm-int 0)
                     (vm-nil) (vm-nil) (vm-nil) (vm-nil)])))
    (is (= (apply-ops st [(op-setarg1 1)])
           (assoc st
             :sp 13
             :stack [(vm-int 999) (vm-int 2)
                     (vm-nil) (vm-nil) (vm-nil) (vm-nil)
                     (vm-codeofs 0x20)
                     (vm-codeofs 0x10)
                     (vm-int 2)
                     (vm-int 0)
                     (vm-nil) (vm-nil) (vm-nil) (vm-nil)])))))