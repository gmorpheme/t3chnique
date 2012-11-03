(ns t3chnique.vm
  (:use [t3chnique.primitive])
  (:use [clojure.algo.monads :only [state-m domonad with-monad fetch-val update-val]])
  (:require [t3chnique.ber :as ber]
            [t3chnique.image :as im]
            [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc])
  (:import [t3chnique.metaclass TadsObject]))


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

#_(defmacro with-stack 
    "Bind symbols to values popped of top of stack and push result of exprs back on"
    [syms & exprs]
  (let [binding-vector (vec (interleave (reverse syms) (repeat '(stack-pop))))]
    `(let ~binding-vector
       (doseq [v# (do ~@exprs)] (stack-push v#)))))

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

(defn ip-position [{:keys [code-page-size code ip]}]
  [(:bytes (nth (/ ip code-page-size) code)) (mod ip code-page-size)])

(defmacro with-buffer [bsym s & exprs]
  `(let [[b# o#] (ip-position ~s)
         ~bsym (.slice b#)
         _# (.position ~bsym o#)]
     ~@exprs))

(defn bump-ip [s count]
  (update-in s :ip (partial + count)))

(defn code-read-ubyte []
  (fn [s]
    (with-buffer buf s
      [(ber/read-ubyte buf) (bump-ip s 1)])))

(defn code-read-sbyte []
  (fn [s]
    (with-buffer buf s
      [(ber/read-sbyte buf) (bump-ip s 1)])))

(defn code-read-uint2 []
  (fn [s]
    (with-buffer buf s
      [(ber/read-uint2 buf) (bump-ip s 2)])))

(defn code-read-int2 []
  (fn [s]
    (with-buffer buf s
      [(ber/read-int2 buf) (bump-ip s 2)])))

(defn code-read-uint4 []
  (fn [s]
    (with-buffer buf s
      [(ber/read-uint4 buf) (bump-ip s 4)])))

(defn code-read-int4 []
  (fn [s]
    (with-buffer buf s
      [(ber/read-int4 buf) (bump-ip s 4)])))

(defn code-read-utf8 [count]
  (fn [s]
    (with-buffer buf s
      [(ber/read-utf8 count) (bump-ip s count)])))

(defn code-read-pref-utf8 []
  (fn [s]
    (with-buffer buf s
      (let [count (ber/read-uint2 buf)]
        [(ber/read-utf8 count) (bump-ip s count)]))))

(defn code-read-data-holder []
  (fn [s]
    (with-buffer buf s
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

(defn stack-push [val]
  (fn [s] [nil (-> s
                  (update-in [:stack] conj val)
                  (update-in [:sp] inc))]))

(defn stack-pop []
  (fn [s] [(last (:stack s))
          (-> s
              (update-in [:stack] pop)
              (update-in [:sp] dec))]))

(defn reg-get [k]
  (fn [s] [(k s) s]))

(defn obj-store [o]
  (fn [s]
    (let [oid (:next-oid s)]
      [(vm-obj oid) (-> s
                        (update-in [:next-oid] inc)
                        (assoc-in [:objs oid] o))])))

(defn obj-retrieve [oid]
  (fn [s] [(get-in s [:objs oid]) s]))

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

(defop pushstri 0x0C [:pref-utf8 string_bytes]
  )

(defop pushparlst 0x0D [:ubyte fixed_arg_count])
(defop makelstpar 0x0E [])

(defop pushenum 0x0F [:int4 val]
  (stack-push (vm-enum-id val)))

(defop pushbifptr 0x10 [:uint2 function_index :uint2 set_index])
(defop neg 0x20 [])
(defop bnot 0x21 [])

(defn- convert-to-string [x]
  (condp = (typeid x)
    vm-nil-id "nil"
    vm-true-id "true"
    vm-int-id (str (value x))
    vm-sstring-id (value x)
    ))

(defn- add-entries [a b]
  (condp = (typeid a)
    vm-int-id (if (vm-int? b)
                (vm-int (+ (value a) (value b)))
                #_(raise NUM_VAL_REQD))
    vm-sstring-id (str (value a) (convert-to-string b))
    ;:else (raise BAD_TYPE_ADD)
    ))

(defn- sub-entries [a b]
  (condp = (typeid a)
    vm-int-id (if (vm-int? b)
                (vm-int (- (value a) (value b)))
                #_ (raise NUM_VAL_REQD))))

(defop add 0x22 []
  (domonad state-m
           [[val1 val2] (m-seq [(stack-pop) (stack-pop)])
            _ (stack-push (add-entries val1 val2))] nil))

(defop sub 0x23 []
  (domonad state-m
           [[val1 val2] (m-seq (repeat 2 (stack-pop)))
            _ (stack-push (sub-entries val1 val2))] nil))

(defop mul 0x24 [])
(defop shl 0x27 [])
(defop ashr 0x28 [])
(defop xor 0x29 [])
(defop lshr 0x30 [])
(defop div 0x2A [])
(defop mod 0x2B [])

(comment (defn not-conv [val]
           (condp = (typeid val)
             vm-nil-id true
             vm-true-id false
             vm-int-id (zero? val)
             vm-prop-id false
             vm-obj-id false
             vm-funcptr-id false
             vm-sstring-id false
             vm-list-id false
             vm-enum-id false
             :else (raise NO_LOG_CONV))))

(defop not 0x2c []
  #_(with-stack [val]
    [(vm-bool (not-conv val))]))

(defop boolize 0x2d []
  #_(with-stack [val]
    [(vm-bool (not (not-conv val)))]))

(defop inc 0x2e []
  #_(with-stack [val]
    [(add-entries [val 1])]))

(defop dec 0x2f []
  #_(with-stack [val]
    [(add-entries [val (- 1)])]))

(comment (defn- valeq [val1 val2]
           (condp = (typeid val1)
             vm-nil-id  (vm-nil? val2)
             vm-true-id (vm-true? val2)
             vm-int-id  (and (vm-int? val2) (= (value val1) (value val2)))
             vm-prop-id (raise TODO)
             vm-enum-id (and (vm-enum? val2) (= (value val1) (value val2)))
             vm-sstring-id (raise TODO)
             vm-list-id (raise TODO)
             vm-codeofs-id (raise TODO)
             vm-obj-id (raise TODO)
             :else false)))

(defop eq 0x40 []
  #_(with-stack [val1 val2]
    [(vm-bool (valeq val1 val2))]))

(defop ne 0x41 []
  #_(with-stack [val1 val2]
    [(vm-bool (not (valeq val1 val2)))]))

(defop lt 0x42 [])
(defop le 0x43 [])
(defop gt 0x44 [])
(defop ge 0x45 [])
(defop retval 0x50 [])
(defop retnil 0x51 [])
(defop rettrue 0x52 [])
(defop ret 0x54 [])
(defop namedargptr 0x56 [:ubyte named_arg_count :uint2 table_offset])
(defop namedargtab 0x57 [:named-arg-args args])

(defop call 0x58 [:ubyte arg_count :uint4 func_offset]

  )

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
  #_(with-stack [val4 val3 val2 val1]
    [val2 val1 val4 val3]))

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
  #_(with-stack [val]
    [val val]))

(defop disc 0x89 []
  #_(with-stack [val]
    []))


(defop disc1 0x89 [:ubyte count]
  #_(repeatedly count stack-pop))

(defop getr0 0x8B [])
(defop getdbargc 0x8C [])
(defop swap 0x8D [])
(defop pushctxele 0x8E [:ubyte element])
(defop dup2 0x8F [])
(defop switch 0x90 [:SPECIAL])
(defop jmp 0x91 [:int2 branch_offset])
(defop jt 0x92 [:int2 branch_offset])
(defop jf 0x93 [:int2 branch_offset])
(defop je 0x94 [:int2 branch_offset])
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
(defop nillcl1 0xD8 [:ubyte local_number])
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
