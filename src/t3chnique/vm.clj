(ns t3chnique.vm
  (:use [t3chnique.primitive])
  (:use [clojure.algo.monads :only [state-m domonad with-monad fetch-val set-val update-val m-seq m-when]])
  (:require [t3chnique.ber :as ber]
            [t3chnique.image :as im]
            [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc])
  (:import [t3chnique.metaclass TadsObject]
           [t3chnique.intrinsics t3vm]))

(def vm-m state-m)

(defn abort 
  "Abort if we hit something we haven't implemented yet."
  [msg]
  (throw (RuntimeException. msg)))

(defprotocol ByteCode
  (mnemonic [self])
  (parse-spec [self]))

(defrecord OpCode [code mnemonic parse-spec run-fn]
  ByteCode
  (mnemonic [self]
    (:mnemonic self))
  (parse-spec [self]
    (:parse-spec self)))

(defonce table (atom {}))

(declare runop)

(defmacro defop
  "Define a bytecode instruction"
  [op cd plist & exprs]
  (let [param-defs (partition 2 plist)
        param-types (vec (map first param-defs))
        param-syms (vec (map second param-defs))
        spec (vec (apply concat (for [[t s] param-defs] [t (keyword s)])))
        op-fn-name (symbol (str "op-" op))]
    `(do
       (defn ^{:opcode ~cd} ~op-fn-name ~param-syms
         (runop (fn [] ~@exprs)))
       
       (swap! table assoc ~cd (OpCode. ~cd '~op ~spec ~(symbol (str "op-" op)))))))

(defmacro with-stack 
  "Bind symbols to values popped of top of stack and push result of exprs back on"
  [syms exprs]
  `(domonad vm-m
            [~(vec (reverse syms)) (m-seq ~(vec (repeat (count syms) '(stack-pop))))
             _# (m-seq (map stack-push ~exprs))]
            nil))

;; vm-m implementation
;;
;; vm state:
(defn vm-state []
  {:stack []
   :r0 0 ; ret val
   :ip 0 ; instruction pointer
   :ep 0 ; entry point
   :sp 0 ; stack pointer
   :fp 0 ; frame pointer

   :say-function (vm-nil)
   :say-method (vm-prop)
   
   :savepoint 0
   :savepoint-count 0
   :code []
   :code-page-size 0
   :const []
   :const-page-size 0
   :objs []
   :next-oid 0
   :mcld []
   :fnsd []})

(defn vm-from-image [im]
  (let [load-pages (fn [type]
                     (let [{:keys [page-count page-size]} (first (filter #(and (= (:id %) "CPDF") (= (:pool-id %) type)) im))
                           pages (filter #(and (= (:id %) "CPPG") (= (:pool-id %) type)) im)]
                       [page-size (reduce #(assoc %1 (:pool-index %2) %2) (vec (repeat page-count 0)) pages)]))
        [code-page-size code-pages] (load-pages 1)
        [const-page-size const-pages] (load-pages 2)
        mcld (mc/wire-up-metaclasses (:entries (first (filter #(= (:id %) "MCLD") im))))
        fnsd (:entries (first (filter #(= (:id %) "FNSD") im)))
        objs (apply merge (map #(mc/read-object-block mcld %) (filter #(= (:id %) "OBJS") im)))]
    (assoc (vm-state)
      :code code-pages :code-page-size code-page-size
      :const const-pages :const-page-size const-page-size
      :mcld mcld
      :fnsd fnsd
      :objs objs
      :next-oid (inc (apply max (keys objs))))))

(defn offset [{:keys [code-page-size code ip]} & ptr]
  (let [p (or ptr ip)]
    [(:bytes (nth (/ p code-page-size) code)) (mod p code-page-size)]))
  
(defmacro with-buffer [[bsym s p] & exprs]
  `(let [[b# o#] (offset ~s ~p)
         ~bsym (.slice b#)
         _# (.position ~bsym o#)]
     ~@exprs))

(defn fresh-pc []
  (fn [s] [nil (assoc s :pc (:ip s))]))

(def set-pc (partial set-val :pc))

(defn commit-pc []
  (fn [s] [nil (-> s
                  (assoc :ip (:pc s))
                  (dissoc :pc))]))

(defn- bump-pc [s count]
  (update-in s [:pc] (partial + count)))

(def get-say-method (fetch-val :say-method))
(def set-say-method (partial set-val :say-method))
(def get-say-function (fetch-val :say-function))
(def set-say-function (partial set-val :say-function))

(defn code-read-ubyte []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-ubyte buf) (bump-pc s 1)])))

(defn code-read-sbyte []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-sbyte buf) (bump-pc s 1)])))

(defn code-read-uint2 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-uint2 buf) (bump-pc s 2)])))

(defn code-read-int2 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-int2 buf) (bump-pc s 2)])))

(defn code-read-uint4 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-uint4 buf) (bump-pc s 4)])))

(defn code-read-int4 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-int4 buf) (bump-pc s 4)])))

(defn code-read-utf8 [count]
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-utf8 count) (bump-pc s count)])))

(defn code-read-pref-utf8 []
  (fn [s]
    (with-buffer [buf s]
      (let [count (ber/read-uint2 buf)]
        [(ber/read-utf8 count) (bump-pc s count)]))))

(defn code-read-data-holder []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-data-holder buf) (bump-pc s 5)])))

(defn code-read-item [type-sym]
  (condp = type-sym
    :uint2 (code-read-uint2)
    :int2 (code-read-int2)
    :uint4 (code-read-uint4)
    :int4 (code-read-int4)
    :ubyte (code-read-ubyte)
    :sbyte (code-read-sbyte)
    :data-holder (code-read-data-holder)
    :pref-utf8 (code-read-pref-utf8)
    (code-read-utf8 (second type-sym))))

(defn get-method-header
  "Read method header at specified offset."
  [ptr]
  (fn [s]
    (with-buffer [buf s ptr]
      [(im/read-method-header buf) s])))

(defn get-exception-table
  "Read exception table at specified offset."
  [ptr]
  (fn [s]
    (with-buffer [buf s ptr]
      (im/read-exception-table buf) s)))

(defn stack-push [val]
  (fn [s] [nil (-> s
                  (update-in [:stack] conj val)
                  (update-in [:sp] inc))]))

(defn stack-pop []
  (fn [s] [(last (:stack s))
          (-> s
              (update-in [:stack] pop)
              (update-in [:sp] dec))]))

(defn stack-peek []
  (fn [s]
    [(last (:stack s)) s]))

(defn stack-set [idx val]
  (fn [s]
    [nil (assoc-in s [:stack idx] val)]))

(defn stack-get [idx]
  (fn [s]
    [(nth (:stack s) idx) s]))

(defn stack-update [idx f]
  (fn [s]
    (let [stack (:stack s)
          [pre [x & post]] (split-at idx stack)
          stack (concat pre (cons x post))]
      [nil (assoc s :stack stack)])))

(defn stack-swap [i j]
  (fn [s]
    (let [stack (:stack s)
          [i j] (sort [i j])
          [prei [iv & posti]] (split-at i stack)
          [prej [jv & postj]] (split-at j posti)
          stack (concat prei [jv] prej [iv] postj)]
      [nil (assoc s :stack stack)])))

(defn pc []
  (fn [s] [(:pc s) s]))

(def reg-get fetch-val)
(def reg-set set-val)

(defn obj-store [o]
  (fn [s]
    (let [oid (:next-oid s)]
      [(vm-obj oid) (-> s
                        (update-in [:next-oid] inc)
                        (assoc-in [:objs oid] o))])))

(defn obj-retrieve [oid]
  (fn [s] [(get-in s [:objs oid]) s]))

(defn jump [offset]
  (fn [s] [nil (bump-pc s (- offset 2))]))

;; The main step function
(defn runop
  "Sets up program counter for a single operations implementation and handles
   exceptions, rollback etc."
  [op]
  (with-monad vm-m (m-seq [(fresh-pc) (op) (commit-pc)])))

;; Operations on primitives / op overloads

; TODO other types
(defn- convert-to-string [x]
  (cond
    vm-nil? "nil"
    vm-true? "true"
    vm-int? (str (value x))
    vm-sstring? (value x)
    :else (abort "other types")))

; TODO non-numerics
(defn- vm-lift2
  ([op retf]
     (fn [a b]
       (cond
        (vm-int? a) (if (vm-int? b)
                      (retf (op (value a) (value b)))
                      (abort "invalid numeric op"))
        :else (abort "NUM_VAL_REQD"))))
  ([op]
     (vm-lift2 op identity)))

(defn- vm-lift1
  ([op retf]
     (fn [a]
       (cond
        (vm-int? a) (retf (op (value a)))
        :else (abort "NUM_VAL_REQD")))))

;; comparisons operating on type values but returning raw booleans

(def vm->? (vm-lift2 >))
(def vm->=? (vm-lift2 >=))
(def vm-<? (vm-lift2 <))
(def vm-<=? (vm-lift2 <=))

(def vm-> (vm-lift2 > vm-bool))
(def vm->= (vm-lift2 >= vm-bool))
(def vm-< (vm-lift2 < vm-bool))
(def vm-<= (vm-lift2 <= vm-bool))

;; operations operating on type values and returing typed values

; TODO non numerics
(defn- vm-+ [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                (vm-int (+ (value a) (value b)))
                (abort "NUM_VAL_REQD"))
   (vm-sstring? a) (str (value a) (convert-to-string b))
   :else (abort "BAD_TYPE_ADD")))

;; TODO non numerics

(def vm-- (vm-lift2 - vm-int))
(def vm-* (vm-lift2 * vm-int))
(def vm-div (vm-lift2 / vm-int)) ; TODO div by zero
(def vm-mod (vm-lift2 mod vm-int)) ; TODO div by zero
(def vm-<< (vm-lift2 bit-shift-left vm-int))
(def vm->> (vm-lift2 bit-shift-right vm-int))
(def vm-inc (vm-lift1 inc vm-int))
(def vm-dec (vm-lift1 dec vm-int))

;; Clojure doesn't expose java's >>>, this is from http://pastebin.com/4PgeHmPJ
(defn logical-shift-right [n s] 
  (if (neg? n) 
    (bit-or (bit-shift-right (bit-and n 0x7fffffff) s)
            (bit-shift-right 0x40000000 (dec s)))
    (bit-shift-right n s)))

(def vm->>> (vm-lift2 logical-shift-right vm-int))
(def vm-xor (vm-lift2 bit-xor vm-int))

(defn- vm-falsey? [val]
  (or (vm-nil? val) (and (vm-int? val) (vm-zero? val))))

; TODO non numeric
(defn vm-eq? [a b]
  (cond
   (vm-nil? a) (vm-nil? b)
   (vm-true? a) (vm-true? b)
   (vm-int? a) (and (vm-int? b) (= (value a) (value b)))
   (vm-enum? a) (and (vm-enum? b (= (value a) (value b))))
   :else (abort "eq not implemented for type")))

;;; Op codes

(defop push_0 0x01 []
  (stack-push (vm-int 0)))

(defop push_1 0x02 []
  (stack-push (vm-int 1)))

(defop pushint8 0x03 [:sbyte val]
  (stack-push (vm-int val)))

(defop pushint 0x04 [:int4 val]
  (stack-push (vm-int val)))

(defop pushstr 0x05 [:uint4 offset]
  (stack-push (vm-sstring offset)))

(defop pushlst 0x06 [:uint4 offset]
  (stack-push (vm-list offset)))

(defop pushobj 0x07 [:uint4 objid]
  (stack-push (vm-obj objid)))

(defop pushnil 0x08 []
  (stack-push (vm-nil)))

(defop pushtrue 0x09 []
  (stack-push (vm-true)))

(defop pushpropid 0x0A [:uint2 propid]
  (stack-push (vm-prop propid)))

(defop pushfnptr 0x0B [:uint4 code_offset]
  (stack-push (vm-funcptr-id code_offset)))

; TODO
(defop pushstri 0x0C [:pref-utf8 string_bytes])

; TODO
(defop pushparlst 0x0D [:ubyte fixed_arg_count])

; TODO
(defop makelstpar 0x0E [])

(defop pushenum 0x0F [:int4 val]
  (stack-push (vm-enum val)))

; TODO
(defop pushbifptr 0x10 [:uint2 function_index :uint2 set_index])

; TODO non-numerics
(defop neg 0x20 []
  (with-stack [val]
    [(if (vm-int? val) (vm-int (- (value val))) (abort "non-numerics"))]))

; TODO make this work properly
(defop bnot 0x21 []
  (with-stack [val]
    [(if (vm-int? val) (vm-int (bit-not (value val))) (abort "non-numerics"))]))

(defn- stack-op1 [op]
  (with-stack [val] [(op val)]))

(defn- stack-op2 [op]
  (with-stack [a b] [(op a b)]))

(defop add 0x22 []
  (stack-op2 vm-+))

(defop sub 0x23 []
  (stack-op2 vm--))

(defop mul 0x24 []
  (stack-op2 vm-*))

(defop shl 0x27 []
  (stack-op2 vm-<<))
  
(defop ashr 0x28 []
  (stack-op2 vm->>))

(defop xor 0x29 []
  (stack-op2 vm-xor))

(defop lshr 0x30 []
  (stack-op2 vm->>>))

(defop div 0x2A []
  (stack-op2 vm-div))

(defop mod 0x2B []
  (stack-op2 vm-mod))

(defop not 0x2c []
  (with-stack [val]
    [(vm-bool (vm-falsey? val))]))

(defop boolize 0x2d []
  (with-stack [val]
    [(vm-bool (not (vm-falsey? val)))]))

(defop inc 0x2e []
  (stack-op1 vm-inc))

(defop dec 0x2f []
  (stack-op1 vm-dec))

(defop eq 0x40 []
  (with-stack [a b] [(vm-bool (vm-eq? a b))]))

(defop ne 0x41 []
  (with-stack [a b] [(vm-bool (not (vm-eq? a b)))]))

(defop lt 0x42 []
  (stack-op2 vm-<))

(defop le 0x43 []
  (stack-op2 vm-<=))

(defop gt 0x44 []
  (stack-op2 vm->))

(defop ge 0x45 []
  (stack-op2 vm->=))

(defn unwind []
  (domonad vm-m
           [sp (reg-get :sp)
            fp (reg-get :fp)
            _ (m-seq (repeat (- sp fp) (stack-pop)))
            fp (stack-pop)
            ac (stack-pop)
            of (stack-pop)
            ep (stack-pop)
            _ (m-seq (repeat (+ 4 (value ac)) (stack-pop)))
            _ (reg-set :fp (value fp))
            _ (reg-set :ep (value ep))
            _ (set-pc (+ (value ep) (value of)))]
           nil))

(defop retval 0x50 []
  (domonad vm-m
           [rv (stack-pop)
            _ (reg-set :r0 rv)
            _ (unwind)]
           nil))

(defop retnil 0x51 []
  (domonad vm-m
           [_ (reg-set :r0 (vm-nil))
            _ (unwind)]
           nil))

(defop rettrue 0x52 []
  (domonad vm-m
           [_ (reg-set :r0 (vm-true))
            _ (unwind)]
           nil))

(defop ret 0x54 []
  (unwind))

; TODO implement
(defop namedargptr 0x56 [:ubyte named_arg_count :uint2 table_offset])

; TODO implement
(defop namedargtab 0x57 [:named-arg-args args])

(defn check-argc [{:keys [param-count opt-param-count]} ac]
  (let [varags (not= (bit-and param-count 0x80) 0)
        varmin (bit-and param-count 0x7f)]
    (if varags
      (>= ac varmin)
      (= ac param-count))))

(defop call 0x58 [:ubyte arg_count :uint4 func_offset]
  (domonad vm-m
           [_ (m-seq (repeat 4 (stack-push (vm-nil))))
            ep (reg-get :ep)
            p (pc)
            fp (reg-get :fp)
            _ (stack-push (vm-codeofs (- p ep)))
            _ (stack-push (vm-codeofs ep))
            _ (stack-push (vm-int arg_count))
            _ (stack-push (vm-int fp))
            sp (reg-get :sp)
            _ (reg-set :fp sp)
            _ (reg-set :ep func_offset)
            mh (get-method-header func_offset)
;            _ (m-when (check-argc mh arg_count))
            _ (m-seq (repeat (:local-variable-count mh) (stack-push (vm-nil))))
            _ (set-pc (+ func_offset (:code-offset mh)))]
           nil))

(defop ptrcall 0x59 [:ubyte arg_count])

(defop getprop 0x60 [:uint2 prop_id]
  (domonad vm-m
           [target-val (stack-pop)
            obj (obj-retrieve target-val)]
           (mc/get-property obj prop_id)))


(defop callprop 0x61 [:ubyte arg_count :uint2 prop_id])
(defop ptrcallprop 0x62 [:ubyte arg_count])
(defop getpropself 0x63 [:uint2 prop_id])
(defop callpropself 0x64 [:ubyte arg_count :uint2 prop_id])
(defop ptrcallpropself 0x65 [:ubyte arg_count])
(defop objgetprop 0x66 [:uint4 obj_id :uint2 prop_id])
(defop objcallprop 0x67 [:ubyte arg_count :uint4 obj_id :uint2 prop_id])
(defop getpropdata 0x68 [:uint2 prop_id])
(defop ptrgetpropdata 0x69 [])
(defop getproplcl1 0x6A [:ubyte local_number :uint2 prop_id])
(defop callproplcl1 0x6B [:ubyte arg_count :ubyte local_number :uint2 prop_id])
(defop getpropr0 0x6C [:uint2 prop_id])
(defop callpropr0 0x6D [:ubyte arg_count :uint2 prop_id])
(defop inherit 0x72 [:ubyte arg_count :uint2 prop_id])
(defop ptrinherit 0x73 [:ubyte arg_count])
(defop expinherit 0x74 [:ubyte arg_count :uint2 prop_id :uint4 obj_id])
(defop ptrexpinherit 0x75 [:ubyte arg_count :uint4 obj_id])
(defop varargc 0x76 [])
(defop delegate 0x77 [:ubyte arg_count :uint2 prop_id])
(defop ptrdelegate 0x78 [:ubyte arg_count])

(defop swap2 0x7a []
  (with-stack [d c b a] [a b c d]))

(defop swapn 0x7b [:ubyte idx1 :ubyte idx2]
  (domonad vm-m
           [sp (reg-get :sp)
            _ (stack-swap (- sp idx1) (- sp idx2))]
           nil))

;; A set of operations which retrieve an item from lower down the
;; stack and push it onto the stack

(defn- copy [reg offsetf]
  (domonad vm-m [fp (reg-get reg)
                 rv (stack-get (offsetf fp))
                 _ (stack-push rv)]
           nil))

(defop getlcl1 0x80 [:ubyte local_number]
  (copy :fp (partial + local_number)))

(defop getlcl2 0x81 [:uint2 local_number]
  (copy :fp (partial + local_number)))

(defop getarg1 0x82 [:ubyte param_number]
  (copy :fp #(- % (+ 9 param_number))))

(defop getarg2 0x83 [:UNIT2 param_number]
  (copy :fp #(- % (+ 9 param_number))))

(defop getargn0 0x7C []
  (copy :fp #(- % 9)))

(defop getargn1 0x7D []
  (copy :fp #(- % 10)))

(defop getargn2 0x7E []
  (copy :fp #(- % 11)))

(defop getargn3 0x7F []
  (copy :fp #(- % 12)))

(defop pushself 0x84 []
  (copy :fp #(- % 5)))

; TODO debugger
(defop getdblcl 0x85 [:uint2 local_number]
  (abort "Not implemented"))

; TODO debugger
(defop getdbarg 0x86 [:uint2 param_number]
  (abort "Not implemented"))

(defop getargc 0x87 []
  (copy :fp #(- % 2)))


;; Simple stack manipulations

(defop dup 0x88 []
  (with-stack [x] [x x]))

(defop disc 0x89 []
  (with-stack [x] []))

(defop disc1 0x89 [:ubyte count]
  (with-monad vm-m (m-seq (repeat count (stack-pop)))))

(defop getr0 0x8B []
  (reg-get :r0))

; TODO debugger
(defop getdbargc 0x8C [])

(defop swap 0x8D []
  (with-stack [x y] [y x]))

; TODO context store / retrieve
(defop pushctxele 0x8E [:ubyte element])

(defop dup2 0x8F []
  (with-stack [a b] [a b a b]))

; TODO switch
(defop switch 0x90 [:SPECIAL])

;; Various jump and branch operations

(defop jmp 0x91 [:int2 branch_offset]
  (jump branch_offset))

(defn- jump-cond1
  [jumpif? branch_offset]
  (domonad vm-m
           [v (stack-pop)
            _ (m-when (jumpif? v) (jump branch_offset))]
           nil))

(defn- jump-cond2
  [jumpif? branch_offset]
   (domonad vm-m
            [v2 (stack-pop)
             v1 (stack-pop)
             _ (m-when (jumpif? v1 v2) (jump branch_offset))]
            nil))

(defop jt 0x92 [:int2 branch_offset]
  (jump-cond1 (complement vm-falsey?) branch_offset))

(defop jf 0x93 [:int2 branch_offset]
  (jump-cond1 vm-falsey? branch_offset))

(defop je 0x94 [:int2 branch_offset]
  (jump-cond2 vm-eq? branch_offset))

(defop jne 0x95 [:int2 branch_offset]
  (jump-cond2 (complement vm-eq?) branch_offset))

(defop jgt 0x96 [:int2 branch_offset]
  (jump-cond2 vm->? branch_offset))

(defop jge 0x97 [:int2 branch_offset]
  (jump-cond2 vm->=? branch_offset))

(defop jlt 0x98 [:int2 branch_offset]
  (jump-cond2 vm-<? branch_offset))

(defop jle 0x99 [:int2 branch_offset]
  (jump-cond2 vm-<=? branch_offset))

(defop jst 0x9A [:int2 branch_offset]
  (domonad vm-m [v (stack-peek)
                 _ (if (not (vm-falsey? v))
                     (jump branch_offset)
                     (stack-pop))]
           nil))

(defop jsf 0x9B [:int2 branch_offset]
  (domonad vm-m [v (stack-peek)
                 _ (if (vm-falsey? v)
                     (jump branch_offset)
                     (stack-pop))]
           nil))

;; Local jumps

(defop ljsr 0x9C [:int2 branch_offset])
(defop lret 0x9D [:int2 local_variable_number])

(defop jnil 0x9E [:int2 branch_offset]
  (jump-cond1 vm-nil? branch_offset))

(defop jnotnil 0x9F [:int2 branch_offset]
  (jump-cond1 (complement vm-nil?) branch_offset))

(defop jr0t 0xA0 [:int2 branch_offset]
  (domonad vm-m [r0 (reg-get :r0)
                 _ (m-when (not (vm-falsey? r0)) (jump branch_offset))]
           nil))

(defop jr0f 0xA1 [:int2 branch_offset]
  (domonad vm-m [r0 (reg-get :r0)
                 _ (m-when (vm-falsey? r0) (jump branch_offset))]
           nil))

(defop getspn 0xA6 [:ubyte index]
  (copy :sp #(- % (inc index))))

(defop getlcln0 0x8AA []
  (copy :fp identity))

(defop getlcln1 0x8AB []
  (copy :fp inc))

(defop getlcln2 0x8AC []
  (copy :fp (partial + 2)))

(defop getlcln3 0x8AD []
  (copy :fp (partial + 3)))

(defop getlcln4 0x8AE []
  (copy :fp (partial + 4)))

(defop getlcln5 0x8AF []
  (copy :fp (partial + 5)))

;; TODO
(defop say 0xB0 [:uint4 offset])

;; TODO bifs
(defop builtin_a 0xB1 [:ubyte argc :ubyte func_index])

;; TODO bifs
(defop builtin_b 0xB2 [:ubyte argc :ubyte func_index])

;; TODO bifs
(defop builtin_c 0xB3 [:ubyte argc :ubyte func_index])

;; TODO bifs
(defop builtin_d 0xB4 [:ubyte argc :ubyte func_index])

;; TODO bifs
(defop builtin1 0xB5 [:ubyte argc :ubyte func_index :ubyte set_index])

;; TODO bifs
(defop builtin2 0xB6 [:ubyte argc :uint2 func_index :ubyte set_index])

(defop callext 0xB7 [] (abort "callext not implemented"))

; TODO implement
(defop throw 0xB8 []
  (domonad vm-m
           [ep (reg-get :ep)
            ip (reg-get :ip)
            mh (get-method-header ep)
            et (:etable-offset mh)
            ex (get-exception-table et)
;            _ (filter #(<= (:first-offset %) (dec ip) (:last-offset)) ex)
            ]
           nil))

;; TODO
(defop sayval 0xB9 [])

;; TODO
(defop index 0xBA [])

;; TODO
(defop idxlcl1int8 0xBB [:ubyte local_number :ubyte index_val])

;; TODO
(defop idxint8 0xBC [:ubyte index_val])

;; TODO
(defop new1 0xC0 [:ubyte arg_count :ubyte metaclass_id])

;; TODO
(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id])

;; TODO
(defop trnew1 0xC2 [:ubyte arg_count :ubyte metaclass_id])

;; TODO
(defop trnew2 0xC3 [:uint2 arg_count :uint2 metaclass_id])

(defn- setlcl [i v]
  (domonad vm-m [fp (reg-get :fp)
                 _ (stack-set (+ fp i) v)]
           nil))

(defn- updlcl [i f]
  (domonad vm-m [fp (reg-get :fp)
                 v (stack-get (+ fp i))
                 _ (stack-set (+ fp i) (f v))]
           nil))

(defop inclcl 0xD0 [:uint2 local_number]
  (updlcl local_number vm-inc))

; TODO
(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id])

(defop declcl 0xD1 [:uint2 local_number]
  (updlcl local_number vm-dec))

(defop addilcl1 0xD2 [:ubyte local_number :sbyte val]
  (updlcl local_number (partial vm-+ val)))

(defop addilcl4 0xD3 [:uint2 local_number :int4 val]
  (updlcl local_number (partial vm-+ val)))

(defop addtolcl 0xD4 [:uint2 local_number]
  (domonad vm-m [v (stack-pop)
                 _ (updlcl local_number #(vm-+ % v))]
           nil))

(defop subfromlcl 0xD5 [:uint2 local_number]
  (domonad vm-m [v (stack-pop)
                 _ (updlcl local_number #(vm-- % v))]
           nil))

(defop zerolcl1 0xD6 [:ubyte local_number]
  (setlcl local_number (vm-int 0)))

(defop zerolcl2 0xD7 [:uint2 local_number]
  (setlcl local_number (vm-int 0)))

(defop nillcl1 0xD8 [:ubyte local_number]
  (setlcl local_number (vm-nil)))

(defop nillcl2 0xD9 [:uint2 local_number]
  (setlcl local_number (vm-nil)))

(defop onelcl1 0xDA [:ubyte local_number]
  (setlcl local_number (vm-int 1)))

(defop onelcl2 0xDB [:uint2 local_number]
  (setlcl local_number (vm-int 1)))

(defop setlcl1 0xE0 [:ubyte local_number]
  (domonad vm-m [v (stack-pop)
                 _ (setlcl local_number v)]
           nil))

(defop setlcl2 0xE1 [:uint2 local_number]
  (domonad vm-m [v (stack-pop)
                 _ (setlcl local_number v)]
           nil))

(defop setarg1 0xE2 [:ubyte arg_number]
  (domonad vm-m [fp (reg-get :fp)
                 v (stack-pop)
                 _ (stack-set (- fp (+ 9 arg_number)) v)]
           nil))

(defop setarg2 0xE3 [:uint2 arg_number]
  (domonad vm-m [fp (reg-get :fp)
                 v (stack-pop)
                 _ (stack-set (- fp (+ 9 arg_number)) v)]
           nil))

(defop setind 0xE4 [])
(defop setprop 0xE5 [:uint2 prop_id])
(defop ptrsetprop 0xE6 [])
(defop setpropself 0xE7 [:uint2 prop_id])
(defop objsetprop 0xE8 [:uint4 obj :uint2 prop_id])
(defop setdblcl 0xE9 [:uint2 local_number])
(defop setdbarg 0xEA [:uint2 param_number])
(defop setself 0xEB [])
(defop loadctx 0xEC [])
(defop storectx 0xED [])
(defop setlcl1r0 0xEE [:ubyte local_number])
(defop setindlcl1i8 0xEF [:ubyte local_number :ubyte index_val])

;; TODO debugger
(defop bp 0xF1 [])

(defop nop 0xF2 [])


;;;;;;;; intrinsic impls


(deftype TadsT3 []
  t3vm
  (t3RunGC [_]
    (with-monad vm-m (m-result nil)))
  (t3SetSay [_ n]
    (let [no-method 2
          no-func   1
          in (fn [v] (if (vm-int? v)
                      (condp = (value v)
                        no-func (vm-nil)
                        no-method (vm-prop)
                        :else (abort "VMERR_BAD_TYPE_BIF"))))
          out (fn [v] (condp = v
                       (vm-nil) (vm-int no-func)
                       (vm-prop) (vm-int no-method)
                      :else v))]
      (domonad vm-m
               [val (stack-pop)
                val' (in val)
                current (if (vm-prop? val') (get-say-method) (get-say-function))
                _ (if (vm-prop? val') (set-say-method val') (set-say-function val'))
                _ (reg-set :r0 (out current))]
               nil)))
  (t3GetVMVsn [_]
    (domonad vm-m [_ (reg-set :r0 (vm-int 0x00000001))] nil))
  (t3GetVMID [_]
    (domonad vm-m [_ (reg-set :r0 (vm-sstring "t3chnique"))] nil))
  (t3GetVMBanner [_]
    (domonad vm-m [_ (reg-set :r0 (vm-sstring "T3chnique Experimental TADS 3 VM - Copyright 2012 Greg Hawkins"))] nil))
  (t3GetVMPreinitMode [_]
    (domonad vm-m [_ (reg-set :r0 (vm-nil))] nil))
  (t3DebugTrace [_ mode & args]
    (domonad vm-m [_ (reg-set :r0 (vm-nil))] nil))
  ;; TODO symbol table access
  (t3GetGlobalSymbols [_ which?])
  ;; TODO prop allocation
  (t3AllocProp [_])
  ;; TODO exceptions
  (t3GetStackTrace [_ level? flags?])
  ;; TODO named args
  (t3GetNamedArg [_ name defval?])
  ;; TODO named args
  (t3GetNamedArgList [_]))