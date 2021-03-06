(ns t3chnique.vm-test
  (:use [t3chnique.vm]
        [t3chnique.primitive]
        [t3chnique.util]
        [midje.sweet])
  (:require [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc]
            [monads.core :as m]))

(facts "about truthiness"
  (vm-truthy? (vm-nil)) => false
  (vm-truthy? (vm-int 0)) => false
  (vm-falsey? (vm-nil)) => true
  (vm-falsey? (vm-int 0)) => true
  (vm-truthy? (vm-int 1)) => true
  (vm-falsey? (vm-int 1)) => false)

(facts "Simple pushes"
  (tabular
   (fact (apply stack-after ?ops) => ?stack)
   
   ?ops                         ?stack
   [(op push_0)]                (st 0)
   [(op push_1)]                (st 1)
   [(op pushint8 100)]          (st 100)
   [(op pushint 123456)]        (st 123456)
   [(op pushlst 0xff)]          [(vm-list 0xff)]
   [(op pushnil) (op pushtrue)] (st nil true)
   [(op pushenum 9876)]         [(vm-enum 9876)]))


(facts "Simple stack ops"
  (tabular
   (fact  (apply-with-stack ?before ?ops) => ?after)

   ?before        ?ops             ?after
   (st 10)        [(op dup)]       (st 10 10)
   (st nil)       [(op dup)]       (st nil nil)
   (st true)      [(op disc)]      (st)
   (st 100 99 98) [(op disc)]      (st 100 99)
   (st nil nil)   [(op disc1 2)]   (st)
   (st nil nil)   [(op disc1 1)]   (st nil)
   (st 1 2)       [(op swap)]      (st 2 1)
   (st 1 2)       [(op dup2)]      (st 1 2 1 2)))


(facts "about arithmetic op codes"
  (fact "Integer increment"
    (doseq [i (range 100)]
      (stack-after (op pushint i) (op inc)) => [(vm-int (inc i))]))

  (fact "Integer decrement"
    (doseq [i (range 100)]
      (stack-after (op pushint i) (op dec)) => [(vm-int (dec i))]))

  (fact "Integer addition"
    (doseq [i [1 2] j [1 2]]
      (stack-after (op pushint8 i) (op pushint8 j) (op add)) => [(vm-int (+ i j))]))

  (fact "Integer equality"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op eq)) => [(vm-bool (= i j))]))

  (fact "Integer inequality"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op ne)) => [(vm-bool (not= i j))]))

  (fact "Integer subtraction"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op sub)) =>  [(vm-int (- i j))]))

  (fact "Integer multiplication"
    (doseq [i (range 10) j (range 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op mul)) => [(vm-int (* i j))]))

  (fact "Integer division"
    (doseq [i (range 10) j (range 1 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op div)) => [(vm-int (/ i j))]))  

  (fact "Integer modulo"
    (doseq [i (range 10) j (range 1 10)]
      (stack-after (op pushint8 i) (op pushint8 j) (op mod)) => [(vm-int (mod i j))]))

  (fact "Negation"
    (doseq [i (range 100)]
      (stack-after (op pushint8 i) (op neg)) => [(vm-int (- i))])))

(facts "Logical operations"
  (tabular
   (fact (apply stack-after ?ops) => ?stack)
   ?ops                         ?stack
   [(op pushnil) (op not)]      (st true)
   [(op pushtrue) (op not)]     (st nil)
   [(op pushint8 0) (op not)]   (st true)
   [(op pushint8 1) (op not)]   (st nil)
   [(op pushint 0) (op not)]    (st true)
   [(op pushint 1) (op not)]    (st nil)
   [(op pushnil) (op boolize)]  (st nil)
   [(op pushtrue) (op boolize)] (st true)
   [(op pushint8 0) (op not)]   (st true)
   [(op pushint8 1) (op not)]   (st nil)
   [(op pushint 0) (op not)]    (st true)
   [(op pushint 1) (op not)]    (st nil)))

(fact "bnot"
  (stack-after (op pushint 0xffffffff) (op bnot)) => (st 0))

(fact "Call stack construction"
  (let [vm (vm-state-with :ep 0x10 :ip 0x30 :pc 0x36 :fp 0 :sp 2 :stack [(vm-int 1) (vm-int 2)])]
    (apply-ops vm [(op call 2 0x1234)]) =>
    (assoc vm
      :ep 0x1234
      :pc 0x123e
      :ip 0x30
      :fp 13
      :sp 17
      :sequence 1
      :stack (st 1 2 (vm-prop 0) nil nil nil nil nil
                 (vm-codeptr nil)
                 (vm-codeofs 0x26)
                 (vm-codeofs 0x10)
                 2 (vm-stack 0) nil nil nil nil))
    (provided
      (get-method-header 0x1234) => (m/return {:param-count 2 :opt-param-count 0 :local-variable-count 4 :code-offset 10}))))

(facts "about function return op codes"

  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 13
                          :stack (st 1 ;arg
                                     2 ;arg
                                     nil ;pid
                                     nil ;tgt
                                     nil ;def
                                     nil ;slf
                                     nil ;ivk
                                     nil ;sfr
                                     (vm-codeptr nil) ;rc
                                     (vm-codeofs 0x20) ;pc
                                     (vm-codeofs 0x10) ;ep
                                     2 ;args
                                     0 ;fp
                                     nil ;sp
                                     nil nil nil 99))]
    (fact (apply-ops vm [(op retval)]) => (vm-state-with :ep 0x20 :ip 0x123e :pc 0x30 :r0 (vm-int 99) :sequence 1)))

  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 13
                          :stack (st 1 2 nil nil nil nil nil nil
                                     (vm-codeptr nil)
                                     (vm-codeofs 0x20)
                                     (vm-codeofs 0x10)
                                     2 0 nil nil nil nil))]
    (fact (apply-ops vm [(op retnil)]) => (vm-state-with :ep 0x20 :ip 0x123e :pc 0x30 :r0 (vm-nil) :sequence 1))
    (fact (apply-ops vm [(op rettrue)]) => (vm-state-with :ep 0x20 :ip 0x123e :pc 0x30 :r0 (vm-true) :sequence 1))
    (fact (apply-ops (assoc vm :r0 (vm-int 111)) [(op ret)]) => (vm-state-with :ip 0x123e :ep 0x20 :pc 0x30 :r0 (vm-int 111) :sequence 1))))

(fact "Local access"
  (let [stack (map vm-int (range 4))]
    (doseq [i (range 4) f [op-getlcl1 op-getlcl2]]
      (apply-with-stack stack [(f nil i)]) => (conj stack (vm-int i)))))

(fact "Argument access"
  (let [vm (vm-state-with :fp 13
                          :stack (st 101 100 nil nil nil nil (vm-nil) (vm-nil) (vm-codeptr nil) (vm-codeofs 0x20) (vm-codeofs 0x10)
                                     2 0 nil nil nil nil))]
    (doseq [i (range 2) f [op-getarg1 op-getarg2]]
      (apply-ops vm [(f nil i)]) => 
      (contains {:sp 18 :stack (has-suffix (vm-int (+ 100 i)))}))))

(fact "Push self"
  (let [vm (vm-state-with :fp 13
             :stack (st 101 100
                        (vm-prop 10) (vm-obj 0x12) (vm-obj 0x22) (vm-obj 0x33)
                        (vm-nil)
                        (vm-nil)
                        (vm-codeptr nil)
                        (vm-codeofs 0x20)
                        (vm-codeofs 0x10)
                        2 0 nil nil nil nil))]
    (apply-ops vm [(op pushself)]) => (contains {:sp 18 :stack (has-suffix (vm-obj 0x33))})))

(facts "Jumps"
  (let [init 0x66
        offs 0x11
        dest (inc (+ init offs))]

    (tabular
     (fact (apply-ops (vm-state-with :ip init :stack ?stack) [?op]) => ?check)
     ?stack        ?op                ?check
     (st)          (op jmp offs)      (contains {:pc dest})
     (st 0 0)      (op je offs)       (contains {:pc dest})
     (st 0 0)      (op jne offs)      #(not (contains? % #{:pc}))
     (st true)     (op jt offs)       (contains {:pc dest})
     (st nil)      (op jt offs)       #(not (contains? % #{:pc}))
     (st true)     (op jf offs)       #(not (contains? % #{:pc}))
     (st nil)      (op jf offs)       (contains {:pc dest})
     (st nil)      (op jnil offs)     (contains {:pc dest})
     (st nil)      (op jnotnil offs)  #(not (contains? % #{:pc}))
     (st true)     (op jnotnil offs)  (contains {:pc dest})
     (st true)     (op jnil offs)     #(not (contains? % #{:pc})))

    (tabular
     (fact (apply-ops (vm-state-with :ip init :stack ?stack :r0 ?r0) [?op]) => ?check)
     ?stack     ?r0        ?op            ?check
     (st true)  (vm-true)  (op jr0t offs) (contains {:pc dest :r0 (vm-true)})
     (st true)  (vm-true)  (op jr0f offs) #(not (contains? % #{:pc}))
     (st nil)   (vm-nil)   (op jr0f offs) (contains {:pc dest :r0 (vm-nil)})
     (st nil)   (vm-nil)   (op jr0t offs) #(not (contains? % #{:pc})))))

(let [init 0x66
      offs 0x11
      dest (inc (+ init offs))
      x-false (vm-state-with :ip init :stack (st 0 0))
      x-true (vm-state-with :ip init :stack (st 0 1))]
  (tabular
   (fact "Jump and save"
     (apply-ops ?state [?op]) => ?check)
   
   ?state      ?op           ?check
   
   x-false (op jst offs) (contains {:stack (st 0)})
   x-true  (op jst offs) (contains {:pc dest})
   x-false (op jsf offs) (contains {:pc dest})
   x-true  (op jsf offs) (contains {:stack (st 0)})))

(facts "Stack access"
  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 11
                          :stack (st 1 2 nil nil nil nil
                                     (vm-codeptr nil)
                                     (vm-codeofs 0x20)
                                     (vm-codeofs 0x10)
                                     2 0 nil nil nil nil 999))]
    (fact (apply-ops vm [(op setarg1 0)]) => (contains {:stack (has-prefix (st 1 999))}))
    (fact (apply-ops vm [(op setarg1 1)]) => (contains {:stack (has-prefix (st 999 2))}))))

(facts "Set locals"
  (let [vm (vm-state-with :ep 0x1234
                          :ip 0x123e
                          :fp 10
                          :r0 (vm-int 727)
                          :stack (st 1 2
                                  nil nil nil nil
                                  (vm-codeofs 0x20)
                                  (vm-codeofs 0x10)
                                  2 0 nil nil))]
    (fact (apply-ops vm [(op setlcl1r0 0) (op setlcl1r0 1)]) => (contains {:stack (has-suffix (st 727 727))}))))

(facts "Switch offset correction"
  (let [inst {:count 5
              :cases [{:case-branch 32, :case-val {:type 7, :value 1}}
                      {:case-branch 33, :case-val {:type 7, :value 2}}
                      {:case-branch 34, :case-val {:type 7, :value 3}}
                      {:case-branch 35, :case-val {:type 7, :value 4}}
                      {:case-branch 36, :case-val {:type 7, :value 5}}]
              :default 42}]
    (resolve-offsets inst) => {:count 5
                               :cases [{:case-branch 40 :case-val {:type 7, :value 1}}
                                       {:case-branch 48 :case-val {:type 7, :value 2}}
                                       {:case-branch 56 :case-val {:type 7, :value 3}}
                                       {:case-branch 64 :case-val {:type 7, :value 4}}
                                       {:case-branch 72 :case-val {:type 7, :value 5}}]
                               :default 80}))


(let [vm (vm-state-with :stack (st 99 (vm-obj 1))
                        :objs {1 (reify
                                   mc/MetaClass
                                   (mc/set-property [self prop val] (m/return {prop val})))})]
  (fact (apply-ops vm [(op setprop 20)]) => (contains {:objs {1 {20 (vm-int 99)}}})))

(let [vm (vm-state-with :stack (st 99 (vm-obj 1) (vm-prop 20))
                        :objs {1 (reify
                                   mc/MetaClass
                                   (mc/set-property [self prop val] (m/return {prop val})))})]
  (fact (apply-ops vm [(op ptrsetprop)]) => (contains {:objs {1 {20 (vm-int 99)}}})))

(let [vm (vm-state-with :fp 9
                        :stack (st (vm-prop 10) (vm-obj 0x1) (vm-obj 0x1) (vm-obj 0x1)
                                   (vm-nil) (vm-nil)
                                   (vm-codeofs 0x20) (vm-codeofs 0x10)
                                   1 0 689)
                        :objs {1 (reify
                                   mc/MetaClass
                                   (mc/set-property [self prop val] (m/return {prop val})))})]
  (fact (apply-ops vm [(op setpropself 20)]) => (contains {:objs {1 {20 (vm-int 689)}}})))

(let [vm (vm-state-with :stack (st 99)
                        :objs {1 (reify
                                   mc/MetaClass
                                   (mc/set-property [self prop val] (m/return {prop val})))})]
  (fact (apply-ops vm [(op objsetprop 1 20)]) => (contains {:objs {1 {20 (vm-int 99)}}})))

(let [vm (vm-state-with :stack (st 99 (vm-codeofs 99))
                        :objs {1 (reify
                                   mc/MetaClass
                                   (mc/set-property [self prop val] (m/return {prop val})))})]
  (fact (apply-ops vm [(op setprop 20)]) => (throws Exception #"OBJ_VAL_REQUIRED")))

