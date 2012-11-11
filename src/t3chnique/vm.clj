(ns t3chnique.vm
  (:use [t3chnique.primitive])
  (:use [clojure.algo.monads :only [state-m domonad with-monad fetch-val update-val m-seq]])
  (:require [t3chnique.ber :as ber]
            [t3chnique.image :as im]
            [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc])
  (:import [t3chnique.metaclass TadsObject]))

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

;; code pool

(defmacro defop
  "Define a bytecode instruction"
  [op cd plist & exprs]
  (let [param-defs (partition 2 plist)
        param-types (vec (map first param-defs))
        param-syms (vec (map second param-defs))
        spec (vec (apply concat (for [[t s] param-defs] [t (keyword s)])))]
    `(do
       (defn ^{:opcode ~cd} ~(symbol (str "op-" op)) ~param-syms ~@exprs)
       (swap! table assoc ~cd (OpCode. ~cd '~op ~spec ~(symbol (str "op-" op)))))))

(defmacro with-stack 
  "Bind symbols to values popped of top of stack and push result of exprs back on"
  [syms exprs]
  `(domonad state-m
            [~(vec (reverse syms)) (m-seq ~(vec (repeat (count syms) '(stack-pop))))
             _# (m-seq (map stack-push ~exprs))]
            nil))

;; state-m implementation
;;
;; vm state:
(defn vm-state []
  {:stack []
   :r0 0
   :ip 0
   :ep 0
   :sp -1
   :fp 0
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

(defn bump-ip [s count]
  (update-in s :ip (partial + count)))

(defn code-read-ubyte []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-ubyte buf) (bump-ip s 1)])))

(defn code-read-sbyte []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-sbyte buf) (bump-ip s 1)])))

(defn code-read-uint2 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-uint2 buf) (bump-ip s 2)])))

(defn code-read-int2 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-int2 buf) (bump-ip s 2)])))

(defn code-read-uint4 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-uint4 buf) (bump-ip s 4)])))

(defn code-read-int4 []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-int4 buf) (bump-ip s 4)])))

(defn code-read-utf8 [count]
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-utf8 count) (bump-ip s count)])))

(defn code-read-pref-utf8 []
  (fn [s]
    (with-buffer [buf s]
      (let [count (ber/read-uint2 buf)]
        [(ber/read-utf8 count) (bump-ip s count)]))))

(defn code-read-data-holder []
  (fn [s]
    (with-buffer [buf s]
      [(ber/read-data-holder buf) (bump-ip s 5)])))

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

(defn stack-push [val]
  (fn [s] [nil (-> s
                  (update-in [:stack] conj val)
                  (update-in [:sp] inc))]))

(defn stack-pop []
  (fn [s] [(last (:stack s))
          (-> s
              (update-in [:stack] pop)
              (update-in [:sp] dec))]))

(defn stack-set [idx val]
  (fn [s]
    [nil (assoc-in s [:stack idx] val)]))

(defn reg-get [k]
  (fn [s] [(k s) s]))

(defn reg-set [k v]
  (fn [s] [nil (assoc s k v)]))

(defn obj-store [o]
  (fn [s]
    (let [oid (:next-oid s)]
      [(vm-obj oid) (-> s
                        (update-in [:next-oid] inc)
                        (assoc-in [:objs oid] o))])))

(defn obj-retrieve [oid]
  (fn [s] [(get-in s [:objs oid]) s]))

(defn jump [offset]
  (domonad state-m
           [ip (reg-get :ip)
            _ (reg-set :ip (- ip 2) offset)]
           nil))


;; Operations on primitives / op overloads

; TODO other types
(defn- convert-to-string [x]
  (cond
    vm-nil? "nil"
    vm-true? "true"
    vm-int? (str (value x))
    vm-sstring? (value x)
    :else (abort "other types")))

; TODO non numeric
(defn vm-< [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                 (< a b)
                 (abort "invalid comparison"))
   :else (abort "non-numeric")))

; TODO non numeric
(defn vm-> [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                 (> a b)
                 (abort "invalid comparison"))))


; TODO non numerics
(defn- vm-+ [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                (vm-int (+ (value a) (value b)))
                (abort "NUM_VAL_REQD"))
   (vm-sstring? a) (str (value a) (convert-to-string b))
   :else (abort "BAD_TYPE_ADD")))

; TODO non numerics
(defn- vm-- [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                 (vm-int (- (value a) (value b)))
                 (abort "NUM_VAL_REQD"))))

; TODO non numerics
(defn- vm-* [a b]
  (cond
   (vm-int? a) (if (vm-int? b)
                 (vm-int (* (value a) (value b)))
                 (abort "NUM_VAL_REQD"))
   :else (abort "NUM_VAL_REQD")))

(defn- vm-div [a b]
  (cond
   (vm-int? a) (if (= (value b) 0)
                 (abort "DIVIDE_BY_ZERO")
                 (if (vm-int? b)
                   (vm-int (/ (value a) (value b)))
                   (abort "NUM_VAL_REQD")))
   :else (abort "num vals")))

(defn- vm-mod [a b]
  (cond
   (vm-int? a) (if (= (value b) 0)
                 (abort "DIVIDE_BY_ZERO")
                 (if (vm-int? b)
                   (vm-int (mod (value a) (value b)))
                   (abort "NUM_VAL_REQD")))))

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

(defop add 0x22 []
  (with-stack [a b] [(vm-+ a b)]))

(defop sub 0x23 []
  (with-stack [a b] [(vm-- a b)]))

(defop mul 0x24 []
  (with-stack [a b] [(vm-* a b)]))

(defop shl 0x27 [])
(defop ashr 0x28 [])
(defop xor 0x29 [])
(defop lshr 0x30 [])

(defop div 0x2A []
  (with-stack [a b] [(vm-div a b)]))

(defop mod 0x2B []
  (with-stack [a b] [(vm-mod a b)]))

(defop not 0x2c []
  (with-stack [val]
    [(vm-bool (vm-falsey? val))]))

(defop boolize 0x2d []
  (with-stack [val]
    [(vm-bool (not (vm-falsey? val)))]))

; TODO check type
(defop inc 0x2e []
  (with-stack [val]
    [(vm-int (inc (value val)))]))

; TODO check type
(defop dec 0x2f []
  (with-stack [val]
    [(vm-int (dec (value val)))]))

(defop eq 0x40 []
  (with-stack [a b] [(vm-bool (vm-eq? a b))]))

(defop ne 0x41 []
  (with-stack [a b] [(vm-bool (not (vm-eq? a b)))]))

(defop lt 0x42 []
  (with-stack [a b] [(vm-bool (vm-< a b))]))

(defop le 0x43 []
  (with-stack [a b] [(vm-bool (not (vm-> a b)))]))

(defop gt 0x44 []
  (with-stack [a b] [(vm-bool (vm-> a b))]))

(defop ge 0x45 []
  (with-stack [a b] [(vm-bool (not (vm-< a b)))]))

(defop retval 0x50 [])
(defop retnil 0x51 [])
(defop rettrue 0x52 [])
(defop ret 0x54 [])
(defop namedargptr 0x56 [:ubyte named_arg_count :uint2 table_offset])
(defop namedargtab 0x57 [:named-arg-args args])

(defn check-argc [{:keys [param-count opt-param-count]} ac]
  (let [varags (not= (bit-and param-count 0x80) 0)
        varmin (bit-and param-count 0x7f)]
    (if varags
      (>= ac varmin)
      (= ac param-count))))

(defop call 0x58 [:ubyte arg_count :uint4 func_offset]
  (domonad state-m
           [_ (m-seq (repeat 4 (stack-push (vm-nil))))
            ep (reg-get :ep)
            ip (reg-get :ip)
            fp (reg-get :fp)
            _ (stack-push (- ip ep))
            _ (stack-push ep)
            _ (stack-push (vm-int arg_count))
            _ (stack-push fp)
            sp (reg-get :sp)
            _ (reg-set :fp sp)
            _ (reg-set :ep func_offset)
            mh (get-method-header func_offset)
                                        ; :when (check-argc mh arg_count)
            _ (m-seq (repeat (:max-slots mh) (stack-push (vm-nil))))
            _ (reg-set :ip (+ func_offset (:code-offset mh)))]
           nil))

(defop ptrcall 0x59 [:ubyte arg_count])

(defop getprop 0x60 [:uint2 prop_id]
  (domonad state-m
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
  #_(stack-swap (- sp idx1) (- sp idx2)))

(defop getargn0 0x7C [])
(defop getargn1 0x7D [])
(defop getargn2 0x7E [])
(defop getargn3 0x7F [])

(defop getlcl1 0x80 [:ubyte local_number]
  #_(stack-push (stack-get (+ fp local_number))))

(defop getlcl2 0x81 [:uint2 local_number]
  #_(stack-push (stack-get (+ fp local_number))))

(defop getarg1 0x82 [:ubyte param_number]
  #_(stack-push (stack-get (- fp param_number))))

(defop getarg2 0x83 [:UNIT2 param_number]
  #_(stack-push (stack-get (- fp param_number))))

(defop pushself 0x84 [])
(defop getdblcl 0x85 [:uint2 local_number])
(defop getdbarg 0x86 [:uint2 param_number])
(defop getargc 0x87 [])

(defop dup 0x88 []
  (with-stack [x] [x x]))

(defop disc 0x89 []
  (with-stack [x] []))

(defop disc1 0x89 [:ubyte count]
  (with-monad state-m (m-seq (repeat count (stack-pop)))))

(defop getr0 0x8B []
  (reg-get :r0))

(defop getdbargc 0x8C [])
(defop swap 0x8D [])
(defop pushctxele 0x8E [:ubyte element])
(defop dup2 0x8F [])
(defop switch 0x90 [:SPECIAL])

(defop jmp 0x91 [:int2 branch_offset]
  (jump branch_offset))

(defop jt 0x92 [:int2 branch_offset]
  (domonad state-m
           [v (stack-pop)
            _ (if (not (vm-falsey?)) (jump branch_offset) (m-result nil))]
           nil))

(defop jf 0x93 [:int2 branch_offset]
  (domonad state-m
           [v (stack-pop)
            _ (if (vm-falsey?) (jump branch_offset) (m-result nil))]
           nil))

(defop je 0x94 [:int2 branch_offset]
  (domonad state-m
           [v2 (stack-pop)
            v1 (stack-pop)
            _ (if (vm-eq? v1 v2) (jump branch_offset) (m-result nil))]
           nil))

(defop jne 0x95 [:int2 branch_offset])
(defop jgt 0x96 [:int2 branch_offset])
(defop jge 0x97 [:int2 branch_offset])
(defop jlt 0x98 [:int2 branch_offset])
(defop jle 0x99 [:int2 branch_offset])
(defop jst 0x9A [:int2 branch_offset])
(defop jsf 0x9B [:int2 branch_offset])
(defop ljsr 0x9C [:int2 branch_offset])
(defop lret 0x9D [:int2 local_variable_number])
(defop jnil 0x9E [:int2 branch_offset])
(defop jnotnil 0x9F [:int2 branch_offset])
(defop jr0t 0xA0 [:int2 branch_offset])
(defop jr0f 0xA1 [:int2 branch_offset])
(defop getspn 0xA6 [:ubyte index])

(defop getlcln0 0x8AA [])
(defop getlcln1 0x8AB [])
(defop getlcln2 0x8AC [])
(defop getlcln3 0x8AD [])
(defop getlcln4 0x8AE [])
(defop getlcln5 0x8AF [])

(defop say 0xB0 [:uint4 offset])
(defop builtin_a 0xB1 [:ubyte argc :ubyte func_index])
(defop builtin_b 0xB2 [:ubyte argc :ubyte func_index])
(defop builtin_c 0xB3 [:ubyte argc :ubyte func_index])
(defop builtin_d 0xB4 [:ubyte argc :ubyte func_index])
(defop builtin1 0xB5 [:ubyte argc :ubyte func_index :ubyte set_index])
(defop builtin2 0xB6 [:ubyte argc :uint2 func_index :ubyte set_index])
(defop callext 0xB7 [])

(defop throw 0xB8 [])

(defop sayval 0xB9 [])
(defop index 0xBA [])
(defop idxlcl1int8 0xBB [:ubyte local_number :ubyte index_val])
(defop idxint8 0xBC [:ubyte index_val])
(defop new1 0xC0 [:ubyte arg_count :ubyte metaclass_id])
(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id])
(defop trnew1 0xC2 [:ubyte arg_count :ubyte metaclass_id])
(defop trnew2 0xC3 [:uint2 arg_count :uint2 metaclass_id])
(defop inclcl 0xD0 [:uint2 local_number])
(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id])
(defop declcl 0xD1 [:uint2 local_number])
(defop addilcl1 0xD2 [:ubyte local_number :sbyte val])
(defop addilcl4 0xD3 [:uint2 local_number :int4 val])
(defop addtolcl 0xD4 [:uint2 local_number])
(defop subfromlcl 0xD5 [:uint2 local_number])
(defop zerolcl1 0xD6 [:ubyte local_number])
(defop zerolcl2 0xD7 [:uint2 local_number])

(defop nillcl1 0xD8 [:ubyte local_number]
  )
(defop nillcl2 0xD9 [:uint2 local_number])
(defop onelcl1 0xDA [:ubyte local_number])
(defop onelcl2 0xDB [:uint2 local_number])
(defop setlcl1 0xE0 [:ubyte local_number])
(defop setlcl2 0xE1 [:uint2 local_number])
(defop setarg1 0xE2 [:ubyte arg_number])
(defop setarg2 0xE3 [:uint2 arg_number])
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
(defop bp 0xF1 [])
(defop nop 0xF2 [])
