(ns t3chnique.vm-test
  (:use [clojure.algo.monads]
        [t3chnique.vm]
        [t3chnique.primitive]
        [midje.sweet])
  (:require [t3chnique.intrinsics :as bif]))

;; helpers

(defn st [& xs]
  (vec (map #(cond
              (number? %) (vm-int %)
              (string? %) (vm-sstring %)
              (nil? %) (vm-nil)
              (= % true) (vm-true)
              :else %)
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
  (let [vm (apply assoc (vm-state) args)]
    (assoc vm :sp (count (:stack vm)))))

(facts "Simple pushes"
  (tabular
   (fact (apply stack-after ?ops) => ?stack)
   
   ?ops                         ?stack
   [(op-push_0)]                (st 0)
   [(op-push_1)]                (st 1)
   [(op-pushint8 100)]          (st 100)
   [(op-pushint 123456)]        (st 123456)
   [(op-pushlst 0xff)]          [(vm-list 0xff)]
   [(op-pushnil) (op-pushtrue)] (st nil true)
   [(op-pushenum 9876)]         [(vm-enum 9876)]))

(facts "Simple stack ops"
  (tabular
   (fact  (apply-with-stack ?before ?ops) => ?after)

   ?before        ?ops             ?after
   (st 10)        [(op-dup)]       (st 10 10)
   (st nil)       [(op-dup)]       (st nil nil)
   (st true)      [(op-disc)]      (st)
   (st 100 99 98) [(op-disc)]      (st 100 99)
   (st nil nil)   [(op-disc1 2)]   (st)
   (st nil nil)   [(op-disc1 1)]   (st nil)
   (st 1 2)       [(op-swap)]      (st 2 1)
   (st 1 2)       [(op-dup2)]      (st 1 2 1 2)))

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

(facts "Logical operations"
  (tabular
   (fact (apply stack-after ?ops) => ?stack)
   ?ops                         ?stack
   [(op-pushnil) (op-not)]      (st true)
   [(op-pushtrue) (op-not)]     (st nil)
   [(op-pushint8 0) (op-not)]   (st true)
   [(op-pushint8 1) (op-not)]   (st nil)
   [(op-pushint 0) (op-not)]    (st true)
   [(op-pushint 1) (op-not)]    (st nil)
   [(op-pushnil) (op-boolize)]  (st nil)
   [(op-pushtrue) (op-boolize)] (st true)
   [(op-pushint8 0) (op-not)]   (st true)
   [(op-pushint8 1) (op-not)]   (st nil)
   [(op-pushint 0) (op-not)]    (st true)
   [(op-pushint 1) (op-not)]    (st nil)))

(future-fact "bnot"
             (stack-after (op-pushint 0xffffffff) (op-bnot)) => (st 0))

(fact "Call stack construction"
  (let [vm (vm-state-with :ep 0x10 :ip 0x30 :fp 0 :sp 2 :stack [(vm-int 1) (vm-int 2)])]
    (apply-ops vm [(op-call 2 0x1234)]) =>
    (assoc vm
      :ep 0x1234
      :ip 0x123e
      :fp 10
      :sp 14
      :stack (st 1 2 (vm-prop 0) nil nil nil
                 (vm-codeofs 0x20)
                 (vm-codeofs 0x10)
                 2 (vm-stack 0) nil nil nil nil))
    (provided
      (get-method-header 0x1234) =>
      (fn [s] [{:param-count 2 :opt-param-count 0 :local-variable-count 4 :code-offset 10} s]))))

(facts "Returns"
  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 10
                          :stack (st 1 2 nil nil nil nil 
                                     (vm-codeofs 0x20)
                                     (vm-codeofs 0x10)
                                     2 0 nil nil nil nil 99))]
    (fact (apply-ops vm [(op-retval)]) => (vm-state-with :ep 0x20 :ip 0x30 :r0 (vm-int 99))))
  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 10
                          :stack (st 1 2 nil nil nil nil 
                                     (vm-codeofs 0x20)
                                     (vm-codeofs 0x10)
                                     2 0 nil nil nil nil))]
    (fact (apply-ops vm [(op-retnil)]) => (vm-state-with :ep 0x20 :ip 0x30 :r0 (vm-nil)))
    (fact (apply-ops vm [(op-rettrue)]) => (vm-state-with :ep 0x20 :ip 0x30 :r0 (vm-true)))
    (fact (apply-ops (assoc vm :r0 (vm-int 111)) [(op-ret)]) => (vm-state-with :ep 0x20 :ip 0x30 :r0 (vm-int 111)))))

(fact "Local access"
  (let [stack (map vm-int (range 4))]
    (doseq [i (range 4) f [op-getlcl1 op-getlcl2]]
      (apply-with-stack stack [(f i)]) => (conj stack (vm-int i)))))

(fact "Argument access"
  (let [vm (vm-state-with :fp 10
                          :stack (st 101 100 nil nil nil nil (vm-codeofs 0x20) (vm-codeofs 0x10)
                                     2 0 nil nil nil nil))]
    (doseq [i (range 2) op [op-getarg1 op-getarg2]]
      (apply-ops vm [(op i)]) => 
      (contains {:sp 15 :stack (has-suffix (vm-int (+ 100 i)))}))))

(fact "Push self"
  (let [vm (vm-state-with :fp 10
             :stack (st 101 100
                        (vm-prop 10) (vm-obj 0x12) (vm-obj 0x22) (vm-obj 0x33)
                        (vm-codeofs 0x20)
                        (vm-codeofs 0x10)
                        2 0 nil nil nil nil))]
    (apply-ops vm [(op-pushself)]) => (contains {:sp 15 :stack (has-suffix (vm-obj 0x33))})))

(facts "Jumps"
  (let [init 0x66
        offs 0x11
        dest (- (+ init offs) 2)]
    (tabular
     (fact (apply-ops (vm-state-with :ip init :stack ?stack) [?op]) => ?check)
     ?stack        ?op            ?check
     (st)          (op-jmp offs)      (contains {:ip dest})
     (st 0 0)      (op-je offs)       (contains {:ip dest})
     (st 0 0)      (op-jne offs)      (contains {:ip init})
     (st true)     (op-jt offs)       (contains {:ip dest})
     (st nil)      (op-jt offs)       (contains {:ip init})
     (st true)     (op-jf offs)       (contains {:ip init})
     (st nil)      (op-jf offs)       (contains {:ip dest})
     (st nil)      (op-jnil offs)     (contains {:ip dest})
     (st nil)      (op-jnotnil offs)  (contains {:ip init})
     (st true)     (op-jnotnil offs)  (contains {:ip dest})
     (st true)     (op-jnil offs)     (contains {:ip init}))
    (tabular
     (fact (apply-ops (vm-state-with :ip init :stack ?stack :r0 ?r0) [?op]) => ?check)
     ?stack     ?r0        ?op            ?check
     (st true)  (vm-true)  (op-jr0t offs) (contains {:ip dest :r0 (vm-true)})
     (st true)  (vm-true)  (op-jr0f offs) (contains {:ip init :r0 (vm-true)})
     (st nil)   (vm-nil)   (op-jr0f offs) (contains {:ip dest :r0 (vm-nil)})
     (st nil)   (vm-nil)   (op-jr0t offs) (contains {:ip init :r0 (vm-nil)}))))

(facts "Jump and save"
  (let [init 0x66
        offs 0x11
        dest (- (+ init offs) 2)
        false-state (vm-state-with :ip init :stack (st 0 0))
        true-state (vm-state-with :ip init :stack (st 0 1))]
    (tabular
     (fact (apply-ops ?state [?op]) => ?check)
     ?state      ?op           ?check
     false-state (op-jst offs) (contains {:stack (st 0)})
     true-state  (op-jst offs) (contains {:ip dest})
     false-state (op-jsf offs) (contains {:ip dest})
     true-state  (op-jsf offs) (contains {:stack (st 0)}))))

(facts "Stack access"
  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 10
                          :stack (st 1 2 nil nil nil nil
                                     (vm-codeofs 0x20)
                                     (vm-codeofs 0x10)
                                     2 0 nil nil nil nil 999))]
    (fact (apply-ops vm [(op-setarg1 0)]) => (contains {:stack (has-prefix (st 1 999))}))
    (fact (apply-ops vm [(op-setarg1 1)]) => (contains {:stack (has-prefix (st 999 2))}))))

(fact "t3SetSay sets method"
  (:say-method (apply-ops
                (vm-state-with :stack [(vm-prop 0x10)])
                [(bif/t3SetSay (host) 1)]))
  => (vm-prop 0x10))

(fact "t3SetSay sets function"
  (:say-function (apply-ops
                  (vm-state-with :stack [(vm-funcptr 0x20)])
                  [(bif/t3SetSay (host) 1)]))
  => (vm-funcptr 0x20))

(fact "t3SetSay returns existing method"
  (:r0 (apply-ops
        (vm-state-with :say-method (vm-prop 0x30) :stack [(vm-prop 0x50)])
        [(bif/t3SetSay (host) 1)]))

  => (vm-prop 0x30))

(fact "t3SetSay returns existing function"
  (:r0 (apply-ops
        (vm-state-with :say-function (vm-funcptr 0x30) :stack [(vm-funcptr 0x50)])
        [(bif/t3SetSay (host) 1)]))

  => (vm-funcptr 0x30))

(fact "t3SetSay can blank method"
  (:say-method (apply-ops
                (vm-state-with :say-method (vm-prop 0x30) :stack [(vm-int 2)])
                [(bif/t3SetSay (host) 1)]))
  => (vm-prop 0))

(fact "t3SetSay can blank function"
  (:say-function (apply-ops
                  (vm-state-with :say-function (vm-funcptr 0x30) :stack [(vm-int 1)])
                  [(bif/t3SetSay (host) 1)]))
  => (vm-nil))