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

(defprotocol BytePool
  (^byte get-sbyte [self idx])
  (^int get-ubyte [self idx])
  (^short get-int2 [self idx])
  (^int get-uint2 [self idx])
  (^int get-int4 [self idx])
  (^long get-uint4 [self idx]))

(defonce code-pool (ref []))

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

(defmacro defop [op cd plist & exprs]
  "Define a bytecode instruction"
  (let [param-defs (partition 2 plist)
        param-syms (vec (map second param-defs))]
    `(defn ^{:opcode ~cd} ~op ~param-syms (dosync ~@exprs))))

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

(defn add-entries [a b]
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

(defn valeq [val1 val2]
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

(defop swap2 0x7a []
  (with-stack [val4 val3 val2 val1]
    [val2 val1 val4 val3]))

(defop swapn 0x7b [::UBYTE idx1 ::UBYTE idx2]
  (stack-swap (- sp idx1) (- sp idx2)))

(defop dup 0x88 []
  (with-stack [val]
    [val val]))

(defop disc 0x89 []
  (with-stack [val]
    []))

(defop disc1 0x89 [::UBYTE count]
  (repeatedly count stack-pop))

(defop call 0x58 [::UBYTE arg_count ::UINT4 func_offset]
  (repeatedly 4 #(stack-push (vm-nil nil)))
                                        ;
  (stack-push ep)
  (stack-push arg_count)
  (stack-push fp)
  (ref-set fp @sp)
  (ref-set ep func_offset)
  )

(defop getlcl1 0x80 [::UBYTE local_number]
  (stack-push (stack-get (+ fp local_number))))

(defop getlcl2 0x81 [::UINT2 local_number]
  (stack-push (stack-get (+ fp local_number))))

(defop getarg1 0x82 [::UBYTE param_number]
  (stack-push (stack-get (- fp param_number))))

(defop getarg2 0x83 [::UNIT2 param_number]
  (stack-push (stack-get (- fp param_number))))




(comment
  (defop getlcln0)
  (defop getlcln1)
  (defop getlcln2)
  (defop getlcln3)
  (defop getlcln4)
  (defop getlcln5)


  (defop addilcl1)
  (defop addilcl4)
  (defop addtolcl)
  (defop ashr)
  (defop band)
  (defop bnot)
  (defop boolize)
  (defop bor)
  (defop bp)
  (defop builtin_a)
  (defop builtin_b)
  (defop builtin_c)
  (defop builtin_d)
  (defop builtin1)
  (defop builtin2)
  (defop call)
  (defop callext)
  (defop callprop)
  (defop callproplcl1)
  (defop callpropr0)
  (defop callpropself)
  (defop declcl)
  (defop delegate)
  (defop disc)
  (defop disc1)
  (defop div)
  (defop dup2)
  (defop expinherit)
  (defop ge)
  (defop getargc)
  (defop getargn0)
  (defop getargn1)
  (defop getargn2)
  (defop getargn3)
  (defop getdbarg)
  (defop getdbargc)
  (defop getdblcl)
  (defop getprop)
  (defop getpropdata)
  (defop getproplcl1)
  (defop getpropr0)
  (defop getpropself)
  (defop getr0)
  (defop getspn)
  (defop gt)
  (defop inclcl)
  (defop index)
  (defop idxint8)
  (defop idxlcl1int8)
  (defop inherit)
  (defop je)
  (defop jf)
  (defop jge)
  (defop jgt)
  (defop jle)
  (defop jlt)
  (defop jmp)
  (defop jne)
  (defop jnil)
  (defop jnotnil)
  (defop jr0f)
  (defop jr0t)
  (defop jsf)
  (defop jst)
  (defop jt)
  (defop le)
  (defop lshr)
  (defop lt)
  (defop ljsr)
  (defop loadctx)
  (defop lret)
  (defop makelstpar)
  (defop mod)
  (defop mul)
  (defop namedargptr)
  (defop namedargtab)
  (defop neg)
  (defop new1)
  (defop new2)
  (defop nillcl1)
  (defop nillcl2)
  (defop nop)
  (defop not)
  (defop objcallprop)
  (defop objgetprop)
  (defop objsetprop)
  (defop onelcl1)
  (defop onelcl2)
  (defop ptrcall)
  (defop ptrcallprop)
  (defop ptrcallpropself)
  (defop ptrdelegate)
  (defop ptrexpinherit)
  (defop ptrgetpropdata)
  (defop ptrinherit)
  (defop ptrsetprop)
  (defop pushbifptr)
  (defop pushctxele)
  (defop pushenum)
  (defop pushfnptr)
  (defop pushlst 0x06 [::UINT4 offset])
  (defop pushnil)
  (defop pushobj)
  (defop pushparlst)
  (defop pushpropid)
  (defop pushself)
  (defop pushstr 0x05 [::UINT4 offset])
  (defop pushstri)
  (defop pushtrue)
  (defop ret)
  (defop retnil)
  (defop rettrue)
  (defop retval)
  (defop say)
  (defop sayval)
  (defop setarg1)
  (defop setarg2)
  (defop setdbarg)
  (defop setdblcl)
  (defop setind)
  (defop setindlcl1i8)
  (defop setlcl1)
  (defop setlcl1r0)
  (defop setlcl2)
  (defop setprop)
  (defop setpropself)
  (defop setself)
  (defop shl)
  (defop swap)
  (defop storectx)
  (defop subfromlcl)
  (defop trnew1)
  (defop swap2)
  (defop swapn)
  (defop switch)
  (defop xor)
  (defop trnew2)
  (defop throw)
  (defop varargc)
  (defop zerolcl1)
  (defop zerolcl2))
