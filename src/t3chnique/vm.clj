(ns t3chnique.vm)

;; portable data types
(comment
  ::SBYTE
  ::UBYTE
  ::UTF8
  ::INT2
  ::UINT2
  ::INT4
  ::UINT4
  ::DATA_HOLDER
  )

;;

;; primitives
(defrecord StackEntry [type value])

(defmacro defprimitive [name typeid doc encoding]
  `(do (defn ~name [val#] (StackEntry. ~typeid val#))
       (defn ~(symbol (str name "?")) [entry#] (= (:type entry#) ~typeid))
       (def ~(symbol (str name "-id")) ~typeid)))

(defprimitive vm-nil 1 "nil (boolean \"false\" or null pointer)" none)
(defprimitive vm-true 2 "boolean \"true\"" none)
(defprimitive vm-stack 3 "Reserved for implementation use for storing native machine pointers to stack frames (see note below)" none)
(defprimitive vm-codeptr 4 "Reserved for implementation use for storing native machine pointers to code (see note below)" none)
(defprimitive vm-obj 5 "object reference as a 32-bit unsigned object ID number" UINT4)
(defprimitive vm-prop 6 "property ID as a 16-bit unsigned number" UINT2)
(defprimitive vm-int 7 "integer as a 32-bit signed number" INT4)
(defprimitive vm-sstring 8 "single-quoted string; 32-bit unsigned constant pool offset" UINT4)
(defprimitive vm-dstring 9 "double-quoted string; 32-bit unsigned constant pool offset" UINT4)
(defprimitive vm-list 10 "list constant; 32-bit unsigned constant pool offset" UINT4)
(defprimitive vm-codeofs 11 "code offset; 32-bit unsigned code pool offset" UINT4)
(defprimitive vm-funcptr 12 "function pointer; 32-bit unsigned code pool offset" UINT4)
(defprimitive vm-empty 13 "no value (this is useful in some cases to represent an explicitly unused data slot, such as a slot that has never been initialized)" none)
(defprimitive vm-native-code 14 "Reserved for implementation use for storing native machine pointers to native code (see note below)" none)
(defprimitive vm-enum 15 "enumerated constant; 32-bit integer" UINT4)
(defprimitive vm-bifptr 16 "built-in function pointer; 32-bit integer, encoding the function set dependency table index in the high-order 16 bits, and the function's index within its set in the low-order 16 bits." UINT4)
(defprimitive vm-objx 17 "Reserved for implementation use for an executable object, as a 32-bit object ID number (see note below)" UINT4)

(defn vm-string? [x] (some [vm-sstring? vm-dstring?] x))
(defn vm-bool [v] (if v (vm-true true) (vm-nil nil)))
(defn typeid [entry] (:type entry))
(defn value [entry] (:value entry))

;; registers

(defonce r0 (ref 0))
(defonce ip (ref 0))
(defonce ep (ref 0))
(defonce sp (ref -1))
(defonce fp (ref 0))
(defonce savepoint (ref 0))
(defonce savepoint-count (ref 0))

;; stack

(defonce stack (ref []))

(defn stack-push [val]
  (alter sp inc)
  (alter stack conj val))

(defn stack-peek []
  (last @stack))

(defn stack-pop []
  (alter sp dec)
  (let [top (stack-peek)]
    (alter stack pop)
    top))

(defn stack-clear []
  (ref-set stack [])
  (ref-set sp 0))

(defn stack-get [i]
  (@stack i))

(defn stack-swap [i j]
  (alter stack assoc i (@stack j) j (@stack i)))

;; code pool


(defmacro raise [sym] nil)

(defn vm-reset []
  (dosync
   (stack-clear)))

;; instructions

(defn parser-for-args [s]
  )

(defprotocol ByteCode
  (mnenomic [self])
  (run [self args])
  (parse-args [self buffer]))

(defrecord OpCode [code mnenomic run-fn parse-args-fn]
  ByteCode
  (mnenomic [self]
    (:mnenomic self))
  (run [self args]
    (apply (:run-fn self) args))
  (parse-args [self buffer]
    (parse-args-fn buffer)))

(defonce table (atom {}))

(defmacro defop [op cd plist & exprs]
  "Define a bytecode instruction"
  (let [param-defs (partition 2 plist)
        param-types (vec (map first param-defs))
        param-syms (vec (map second param-defs))]
    `(do
       (defn ^{:opcode ~cd} ~(symbol (str "op-" op)) ~param-syms (dosync ~@exprs))
       (swap! table assoc ~cd (OpCode. ~cd '~op  ~(symbol (str "op-" op)) (parser-for-args ~param-types))))))

(defmacro with-stack [syms & exprs]
  "Bind symbols to values popped of top of stack and push result of exprs back on"
  (let [binding-vector (vec (interleave (reverse syms) (repeat '(stack-pop))))]
    `(let ~binding-vector
       (doseq [v# (do ~@exprs)] (stack-push v#)))))

(defop push_0 0x01 []
  (stack-push (vm-int 0)))

(defop push_1 0x02 []
  (stack-push (vm-int 1)))

(defop pushint8 0x03 [::SBYTE val]
  (stack-push (vm-int val)))

(defop pushint 0x04 [::INT4 val]
  (stack-push (vm-int val)))

(defop pushstr 0x05 [::UINT4 offset])
(defop pushlst 0x06 [::UINT4 offset])
(defop pushobj 0x07 [::UINT4 objid])
(defop pushnil 0x08 [])
(defop pushtrue 0x09 [])
(defop pushpropid 0x0A [::UINT2 propid])
(defop pushfnptr 0x0B [::UINT4 code_offset])
(defop pushstri 0x0C [::UINT2 string_length ::UTF8 string_bytes])
(defop pushparlst 0x0D [::UBYTE fixed_arg_count])
(defop makelstpar 0x0E [])
(defop pushenum 0x0F [::INT4 val])
(defop pushbifptr 0x10 [::UINT2 function_index ::UINT2 set_index])
(defop neg 0x20 [])
(defop bnot 0x21 [])

(defn- add-entries [a b]
  (condp = (typeid a)
    vm-int-id (if (vm-int? b)
                (vm-int (+ (value a) (value b)))
                (raise NUM_VAL_REQD))
                                        ; TODO other types
    :else (raise BAD_TYPE_ADD)))

(defop add 0x22 []
  (with-stack [val1 val2]
    [(add-entries val1 val2)]))

(defop sub 0x23 []
  (with-stack [val1 val2]
    [(add-entries val (- val2))]))

(defop mul 0x24 [])
(defop shl 0x27 [])
(defop ashr 0x28 [])
(defop xor 0x29 [])
(defop lshr 0x30 [])
(defop div 0x2A [])
(defop mod 0x2B [])

(defn not-conv [val]
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
      :else (raise NO_LOG_CONV)))

(defop not 0x2c []
  (with-stack [val]
    [(vm-bool (not-conv val))]))

(defop boolize 0x2d []
  (with-stack [val]
    [(vm-bool (not (not-conv val)))]))

(defop inc 0x2e []
  (with-stack [val]
    [(add-entries [val 1])]))

(defop dec 0x2f []
  (with-stack [val]
    [(add-entries [val (- 1)])]))

(defn- valeq [val1 val2]
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
    :else false))

(defop eq 0x40 []
  (with-stack [val1 val2]
    [(vm-bool (valeq val1 val2))]))

(defop ne 0x41 []
  (with-stack [val1 val2]
    [(vm-bool (not (valeq val1 val2)))]))

(defop lt 0x42 [])
(defop le 0x43 [])
(defop gt 0x44 [])
(defop ge 0x45 [])
(defop retval 0x50 [])
(defop retnil 0x51 [])
(defop rettrue 0x52 [])
(defop ret 0x54 [])
(defop namedargptr 0x56 [::UBYTE named_arg_count ::UINT2 table_offset])
(defop namedargtab 0x57 [::SPECIAL args])

(defop call 0x58 [::UBYTE arg_count ::UINT4 func_offset]
  (repeatedly 4 #(stack-push (vm-nil nil)))
                                        ;
  (stack-push ep)
  (stack-push arg_count)
  (stack-push fp)
  (ref-set fp @sp)
  (ref-set ep func_offset)
  )

(defop ptrcall 0x59 [::UBYTE arg_count])
(defop getprop 0x60 [::UINT2 prop_id])
(defop callprop 0x61 [::UBYTE arg_count ::UINT2 prop_id])
(defop ptrcallprop 0x62 [::UBYTE arg_count])
(defop getpropself 0x63 [::UINT2 prop_id])
(defop callpropself 0x64 [::UBYTE arg_count ::UINT2 prop_id])
(defop ptrcallpropself 0x65 [::UBYTE arg_count])
(defop objgetprop 0x66 [::UINT4 obj_id ::UINT2 prop_id])
(defop objcallprop 0x67 [::UBYTE arg_count ::UINT4 obj_id ::UINT2 prop_id])
(defop getpropdata 0x68 [::UINT2 prop_id])
(defop ptrgetpropdata 0x69 [])
(defop getproplcl1 0x6A [::UBYTE local_number ::UINT2 prop_id])
(defop callproplcl1 0x6B [::UBYTE arg_count ::UBYTE local_number ::UINT2 prop_id])
(defop getpropr0 0x6C [::UINT2 prop_id])
(defop callpropr0 0x6D [::UBYTE arg_count ::UINT2 prop_id])
(defop inherit 0x72 [::UBYTE arg_count ::UINT2 prop_id])
(defop ptrinherit 0x73 [::UBYTE arg_count])
(defop expinherit 0x74 [::UBYTE arg_count ::UINT2 prop_id ::UINT4 obj_id])
(defop ptrexpinherit 0x75 [::UBYTE arg_count ::UINT4 obj_id])
(defop varargc 0x76 [])
(defop delegate 0x77 [::UBYTE arg_count ::UINT2 prop_id])
(defop ptrdelegate 0x78 [::UBYTE arg_count])

(defop swap2 0x7a []
  (with-stack [val4 val3 val2 val1]
    [val2 val1 val4 val3]))

(defop swapn 0x7b [::UBYTE idx1 ::UBYTE idx2]
  (stack-swap (- sp idx1) (- sp idx2)))

(defop getargn0 0x7C [])
(defop getargn1 0x7D [])
(defop getargn2 0x7E [])
(defop getargn3 0x7F [])

(defop getlcl1 0x80 [::UBYTE local_number]
  (stack-push (stack-get (+ fp local_number))))

(defop getlcl2 0x81 [::UINT2 local_number]
  (stack-push (stack-get (+ fp local_number))))

(defop getarg1 0x82 [::UBYTE param_number]
  (stack-push (stack-get (- fp param_number))))

(defop getarg2 0x83 [::UNIT2 param_number]
  (stack-push (stack-get (- fp param_number))))

(defop pushself 0x84 [])
(defop getdblcl 0x85 [::UINT2 local_number])
(defop getdbarg 0x86 [::UINT2 param_number])
(defop getargc 0x87 [])

(defop dup 0x88 []
  (with-stack [val]
    [val val]))

(defop disc 0x89 []
  (with-stack [val]
    []))


(defop disc1 0x89 [::UBYTE count]
  (repeatedly count stack-pop))

(defop getr0 0x8B [])
(defop getdbargc 0x8C [])
(defop swap 0x8D [])
(defop pushctxele 0x8E [::UBYTE element])
(defop dup2 0x8F [])
(defop switch 0x90 [::SPECIAL])
(defop jmp 0x91 [::INT2 branch_offset])
(defop jt 0x92 [::INT2 branch_offset])
(defop jf 0x93 [::INT2 branch_offset])
(defop je 0x94 [::INT2 branch_offset])
(defop jne 0x95 [::INT2 branch_offset])
(defop jgt 0x96 [::INT2 branch_offset])
(defop jge 0x97 [::INT2 branch_offset])
(defop jlt 0x98 [::INT2 branch_offset])
(defop jle 0x99 [::INT2 branch_offset])
(defop jst 0x9A [::INT2 branch_offset])
(defop jsf 0x9B [::INT2 branch_offset])
(defop ljsr 0x9C [::INT2 branch_offset])
(defop lret 0x9D [::INT2 local_variable_number])
(defop jnil 0x9E [::INT2 branch_offset])
(defop jnotnil 0x9F [::INT2 branch_offset])
(defop jr0t 0xA0 [::INT2 branch_offset])
(defop jr0f 0xA1 [::INT2 branch_offset])
(defop getspn 0xA6 [::UBYTE index])
(defop getlcln0 0x8AA [])
(defop getlcln1 0x8AB [])
(defop getlcln2 0x8AC [])
(defop getlcln3 0x8AD [])
(defop getlcln4 0x8AE [])
(defop getlcln5 0x8AF [])
(defop say 0xB0 [::UINT4 offset])
(defop builtin_a 0xB1 [::UBYTE argc ::UBYTE func_index])
(defop builtin_b 0xB2 [::UBYTE argc ::UBYTE func_index])
(defop builtin_c 0xB3 [::UBYTE argc ::UBYTE func_index])
(defop builtin_d 0xB4 [::UBYTE argc ::UBYTE func_index])
(defop builtin1 0xB5 [::UBYTE argc ::UBYTE func_index ::UBYTE set_index])
(defop builtin2 0xB6 [::UBYTE argc ::UINT2 func_index ::UBYTE set_index])
(defop callext 0xB7 [])
(defop throw 0xB8 [])
(defop sayval 0xB9 [])
(defop index 0xBA [])
(defop idxlcl1int8 0xBB [::UBYTE local_number ::UBYTE index_val])
(defop idxint8 0xBC [::UBYTE index_val])
(defop new1 0xC0 [::UBYTE arg_count ::UBYTE metaclass_id])
(defop new2 0xC1 [::UINT2 arg_count ::UINT2 metaclass_id])
(defop trnew1 0xC2 [::UBYTE arg_count ::UBYTE metaclass_id])
(defop trnew2 0xC3 [::UINT2 arg_count ::UINT2 metaclass_id])
(defop inclcl 0xD0 [::UINT2 local_number])
(defop new2 0xC1 [::UINT2 arg_count ::UINT2 metaclass_id])
(defop declcl 0xD1 [::UINT2 local_number])
(defop addilcl1 0xD2 [::UBYTE local_number ::SBYTE val])
(defop addilcl4 0xD3 [::UINT2 local_number ::INT4 val])
(defop addtolcl 0xD4 [::UINT2 local_number])
(defop subfromlcl 0xD5 [::UINT2 local_number])
(defop zerolcl1 0xD6 [::UBYTE local_number])
(defop zerolcl2 0xD7 [::UINT2 local_number])
(defop nillcl1 0xD8 [::UBYTE local_number])
(defop nillcl2 0xD9 [::UINT2 local_number])
(defop onelcl1 0xDA [::UBYTE local_number])
(defop onelcl2 0xDB [::UINT2 local_number])
(defop setlcl1 0xE0 [::UBYTE local_number])
(defop setlcl2 0xE1 [::UINT2 local_number])
(defop setarg1 0xE2 [::UBYTE arg_number])
(defop setarg2 0xE3 [::UINT2 arg_number])
(defop setind 0xE4 [])
(defop setprop 0xE5 [::UINT2 prop_id])
(defop ptrsetprop 0xE6 [])
(defop setpropself 0xE7 [::UINT2 prop_id])
(defop objsetprop 0xE8 [::UINT4 obj ::UINT2 prop_id])
(defop setdblcl 0xE9 [::UINT2 local_number])
(defop setdbarg 0xEA [::UINT2 param_number])
(defop setself 0xEB [])
(defop loadctx 0xEC [])
(defop storectx 0xED [])
(defop setlcl1r0 0xEE [::UBYTE local_number])
(defop setindlcl1i8 0xEF [::UBYTE local_number ::UBYTE index_val])
(defop bp 0xF1 [])
(defop nop 0xF2 [])