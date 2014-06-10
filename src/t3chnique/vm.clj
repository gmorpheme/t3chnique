(ns ^{:doc "Main VM implementation including state and ops. Depends on metaclass
and function set protocols."}
    t3chnique.vm
  (:use [t3chnique.primitive]
        [t3chnique.monad])
  (:require [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc]
            [clojure.tools.logging :refer [spy debug info trace error]])
  (:use [clojure.algo.monads :only [state-m domonad with-monad fetch-val set-val update-val m-seq m-when update-state fetch-state m-until m-result m-chain]])
  (:import [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

;; The VM has an awkward circular relationship with string and list
;; metaclasses. This is a temporary workaround for the circular dependencies
(do (in-ns 't3chnique.metaclass.string)
    (clojure.core/declare add-to-str)
    (clojure.core/declare create)
    (in-ns 't3chnique.metaclass.list)
    (clojure.core/declare tads-list)
    (in-ns 't3chnique.vm))

;; vm-m implementation
;;
;; vm state:
(defn vm-state []
  {
   ;; stack and registers
   
   :stack []
   :r0 (vm-nil) ; ret val
   :ip 0 ; instruction pointer
   :ep 0 ; entry point
   :sp 0 ; stack pointer
   :fp 0 ; frame pointer

   :say-function (vm-nil)
   :say-method (vm-prop)

   :savepoint 0
   :savepoint-count 0

   ;; code and constant pools, symbols, index lookups etc.
   
   :code-pages []
   :code-page-size 0
   :const-pages []
   :const-page-size 0
   :objs (sorted-map)
   :next-oid 0
   :mcld []
   :fnsd []
   :symd []

   :exc nil ; exception caused by last action 

   ;; metadata / id

   :id nil ; VM id 
   :sequence 0 ; sequence number in VM history
   })

;; image loading

(defmulti load-image-block
  "Takes block from parsed image and loads the data into VM state. Dispatches
on block type."
  (fn [s b] (:id b)))

(defmethod load-image-block "ENTP" [s b]
  (merge s (dissoc b :flags :id)))

(defmethod load-image-block "FNSD" [s b]
  (assoc s :fnsd (:entries b)))

(defmethod load-image-block "SYMD" [s b]
  (assoc s :symd (:entries b)))

(defmethod load-image-block "CPDF" [s {:keys [pool-id page-count page-size]}]
  (if (= pool-id 1)
    (assoc s :code-page-size page-size :code-page-count page-count :code-pages (vec (repeat page-size nil)))
    (assoc s :const-page-size page-size :const-page-count page-count :const-pages (vec (repeat page-size nil)))))

(defmethod load-image-block "CPPG" [s {:keys [pool-id pool-index] :as page}]
  (if (= pool-id 1)
    (assoc-in s [:code-pages pool-index] page)
    (assoc-in s [:const-pages pool-index] page)))

(defmethod load-image-block "MCLD" [s b]
  (assoc s :mcld (mc/wire-up-metaclasses (:entries b))))

(defmethod load-image-block "OBJS" [s b]
  (let [objs (mc/read-object-block (:mcld s) b)
        _ (trace "loaded objects: " (keys objs))
        mcld (mc/process-intrinsic-class-objects (:mcld s) objs)]
    (assoc
        (merge-with merge s {:objs objs})
      :mcld mcld)))

(defmethod load-image-block "EOF " [s b]
  (assoc s :next-oid (inc (apply max (keys (:objs s))))))

(defmethod load-image-block "MRES" [s b]
  s)

(defn vm-from-image [bs]
  (info "===*** Loading new VM image ***===")
  (reduce load-image-block (vm-state) bs))

(defrecord OpCode [code mnemonic parse-spec run-fn])

(defonce table (atom {}))

(declare runop)

(defmacro defop
  "Define a bytecode instruction. This consists of an execution
function op-xxx which accepts the declared parameters (in addition to
a host argument) and the recording of operation metadata for use by
the VM execution."

  [op cd plist & exprs]
  (let [param-defs (partition 2 plist)
        param-types (vec (map first param-defs))
        param-syms (vec (cons 'host (map second param-defs)))
        spec (vec (apply concat (for [[t s] param-defs] [t (keyword s)])))
        op-fn-name (symbol (str "op-" op))
        exprs (or exprs `((abort (str '~op " not implemented"))))]
    `(do
       (defn ^{:opcode ~cd} ~op-fn-name ~param-syms
         (trace '~op ~(vec (rest param-syms)))
         (runop (fn [] ~@exprs)))

       (swap! table assoc ~cd (OpCode. ~cd '~op ~spec ~(symbol (str "op-" op)))))))

(defmacro with-stack
  "Bind symbols to values popped of top of stack and push result of exprs back on"
  [syms exprs]
  `(do-vm
    [~(vec (reverse syms)) (m-seq ~(vec (repeat (count syms) '(stack-pop))))
     _# (m-seq (map stack-push ~exprs))]
    nil))

(defn offset
  "Translate code pointer to buffer / offset pair."
  ([{:keys [ip] :as vm}]
     (offset vm ip))
  ([{:keys [code-page-size code-pages]} p]
     [(:bytes (nth code-pages (/ p code-page-size))) (mod p code-page-size)]))

(defn const-offset
  "Translate const pointer to buffer / offset pair."
  [{:keys [const-page-size const-pages]} ptr]
  [(:bytes (nth const-pages (/ ptr const-page-size))) (mod ptr const-page-size)])

(defn parse-op
  "Parser for reading op code and args from offset into code pool."
  []
  (domonad parse/byteparser-m
    [opcode (parse/ubyte)
     :let [op (@table opcode)]
     args (parse/spec (:parse-spec op))]
    [op args]))

(defn read-op
  "Read the operation in the supplied buffer position.
Return opcode map, args map and byte length of compete instruction."
  [[b i]]
  (let [[[op args] [b' i']] ((parse-op) [b i])]
    [op args (- i' i)]))

(defn parse-op-at-ip []
  (fn [s]
    [(read-op (offset s (:ip s))) s]))

;; We leave :ip pointing at the current instruction during processing.
;; Should opcodes need to jump, they set :pc instead and this will be
;; committed to :ip at the end of the step.

(def set-pc (partial set-val :pc))
(defn pc []
  (fn [s]
    [(or (:pc s) (:ip s)) s]))

(defn commit-pc
  "Commit the in-progress program counter back to ip."
  []
  (do-vm
   [pc (fetch-val :pc)
    _ (set-val :ip pc)
    _ (update-state #(dissoc % :pc))]
   nil))

(defn dump-state [] (fn [s] (println s) [nil s]))
(defn dump-vals [& keys] (fn [s] (println (select-keys s keys)) [nil s]))
(defn get-say-method [] (fetch-val :say-method))
(def set-say-method (partial set-val :say-method))
(defn get-say-function [] (fetch-val :say-function))
(def set-say-function (partial set-val :say-function))

(defn stack-push [val]
  (do-vm
   [_ (update-val :stack #(conj % val))
    _ (update-val :sp inc)]
   nil))

(defn stack-peek []
  (do-vm
   [stack (fetch-val :stack)]
   (last stack)))

(defn stack-pop []
  (do-vm
   [top (stack-peek)
    _ (update-val :stack pop)
    _ (update-val :sp dec)]
   top))

(defn stack-set [idx val]
  (update-val :stack #(assoc % idx val)))

(defn stack-get [idx]
  (do-vm
   [stack (fetch-val :stack)]
   (nth stack idx)))

(defn stack-update [idx f]
  (update-val :stack #(update-in % [idx] f)))

(defn stack-swap [i j]
  (update-val :stack (fn [stack] (-> stack
                                    (assoc i (stack j))
                                    (assoc j (stack i))))))

(defn jump-from-ip
  "Jump to an offset from the instruction byte code (still stored in :ip) by
setting the program counter (pc) - this will be committed to ip once the
instruction is complete."
  [offset-from-ip]
  (do-vm
   [ip (fetch-val :ip)
    _ (set-pc (+ ip offset-from-ip))]
   nil))

(def reg-get fetch-val)
(defn reg-set [reg val] (fn [s] [nil (assoc s reg val)]))
(def return (partial reg-set :r0))

(defn symbol-value [sym]
  (do-vm
   [stab (fetch-val :symd)]
   (get stab sym)))

(defn new-obj-id []
  (do-vm
   [oid (fetch-val :next-oid)
    _ (update-val :next-oid inc)]
   oid))

(defn obj-store [oid o]
  (trace "obj-store" oid)
  (update-val :objs #(assoc % oid o)))

(defn obj-retrieve [oid]
  {:pre [(number? oid)]}
  (trace "obj-retrieve" oid)
  (m-apply get-in [:objs oid]))

(defn get-method-header
  "Read method header at specified offset."
  [ptr]
  (let [f (fn [x] (first ((parse/method-header (:method-header-size x)) (offset x ptr))))]
    (fn [s]
      [(f s) s])))

(defn get-exception-table
  "Read exception table at specified offset."
  [ptr]
  (m-apply
   #((first ((parse/exception-table)
             (offset % ptr))))))

(defn runop
  "Sets up program counter for a single operations implementation and handles
   exceptions, rollback etc."
  [op]
  (do-vm
    [exc (op)
     _ (update-val :sequence inc)]
    exc))

;; Operations on primitives / op overloads

(defn load-string-constant
  "Read a string (prefixed utf-8) from the constant pool."
  [state address]
  (let [[buf idx] (const-offset state address)]
    (first ((parse/prefixed-utf8) [buf idx]))))

(defn as-string
  "Get actual string from sstring or object string."
  [v]
  (cond
   (vm-sstring? v) (fn [s] [(load-string-constant s (value v)) s])
   (vm-obj? v) (do-vm [obj (obj-retrieve (value v))] (mc/get-as-string obj))))

(defn load-list-constant
  "Read a list (prefixed) from the constant pool."
  [state address]
  (let [[buf idx] (const-offset state address)]
    (first ((parse/lst) [buf idx]))))

(defn as-list
  "Get seq from list or object list"
  [v]
  (cond
   (vm-list? v) (fn [s] [(load-list-constant s (value v)) s])
   (vm-obj? v) (do-vm [obj (obj-retrieve (value v))] (mc/get-as-seq obj))))

;; TODO other types
;; should this be in string metaclass
(defn convert-to-string 
  "Return internal string representation (not vm-sstring)."
  [v]
  (cond
   (vm-sstring? v) (fn [s] [(load-string-constant s (value v)) s])
   (vm-obj? v) (do-vm [obj (obj-retrieve (value v))] (mc/get-as-string obj)) ;TODO metaclass cast_to_string
   (vm-int? v) (in-vm (m-result (str (value v))))
   (vm-nil? v) (in-vm (m-result "nil"))
   (vm-true? v) (in-vm (m-result "true"))
   :else (abort (str "TODO other string conversions: value: " (mnemonise v)))))

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
  (stack-push (vm-funcptr code_offset)))

(defop pushstri 0x0C [:pref-utf8 string_bytes]
  (do-vm
   [s (t3chnique.metaclass.string/create string_bytes)
    oid (new-obj-id)
    obj-store (oid s)]
   (vm-obj s)))

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

(defn vm-bnot [val]
  (if (vm-int? val)
    (vm-int (bit-and 0xffffffff (bit-not (value val))))
    (abort "bnot non-numberics")))

(defn- stack-op1 [op]
  (with-stack [val] [(op val)]))

(defn- stack-op2 [op]
  (with-stack [a b] [(op a b)]))

(defop bnot 0x21 []
  (stack-op1 vm-bnot))

(defn- compute-sum [a b]
  (in-vm
   (cond
    (vm-int? a) (if (vm-int? b)
                  (m-result (vm-int (+ (value a) (value b))))
                  (abort "NUM_VAL_REQD"))
    (vm-sstring? a) (t3chnique.metaclass.string/add-to-str a b)
    (vm-obj? a) (t3chnique.metaclass.string/add-to-str a b)
    ;; create string
    :else (abort "BAD_TYPE_ADD"))))

(defop add 0x22 []
  (do-vm
   [[right left] (m-seq (repeat 2 (stack-pop)))
    ret (compute-sum left right)
    _ (stack-push ret)]
   nil))

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
  (do-vm
   [sp (reg-get :sp)
    fp (reg-get :fp)
    _ (m-seq (repeat (- sp fp) (stack-pop)))
    fp (stack-pop)
    ac (stack-pop)
    of (stack-pop)
    ep (stack-pop) ;; TODO recursive call descriptor too
    _ (m-seq (repeat (+ 6 (value ac)) (stack-pop)))
    _ (reg-set :fp (value fp))
    _ (reg-set :ep (value ep))
    _ (set-pc (+ (value ep) (value of)))]
   nil))

(defop retval 0x50 []
  (do-vm
   [rv (stack-pop)
    _ (return rv)
    _ (unwind)]
   nil))

(defop retnil 0x51 []
  (do-vm
   [_ (return (vm-nil))
    _ (unwind)]
   nil))

(defop rettrue 0x52 []
  (do-vm
   [_ (return (vm-true))
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

(def FRAME-SIZE 10)

(defn- get-stack-self []
  (do-vm
   [fp (reg-get :fp)
    self (stack-get (- fp 7))]
   (if (vm-obj? self) self (vm-obj nil))))

(defn- get-stack-local [i]
  (do-vm
   [fp (reg-get :fp)
    target (stack-get (+ fp i))]
   target))


(defn prepare-frame
  "Prepare stack frame for call. target-pid is (vm-prop), objs are
 (vm-obj) or (vm-nil). Func offset and argc are raw int."
  [target-pid target-obj defining-obj self-obj func-offset argc]
  {:pre [(vm-prop? target-pid)
         (vm-obj-or-nil? target-obj)
         (vm-obj-or-nil? defining-obj)
         (vm-obj-or-nil? self-obj)
         (number? func-offset)
         (number? argc)]}
  (do-vm
   [_ (stack-push target-pid) 
    _ (stack-push target-obj) ; object whose method is called
    _ (stack-push defining-obj) ; defining obj (may be superclass of target)
    _ (stack-push self-obj) ; self (method may belong to delegate)
    _ (stack-push (vm-nil)) ; TODO: "invokee"
    _ (stack-push (vm-nil)) ; TODO: "stack frame references"
    ep (reg-get :ep)
    p (pc)
    fp (reg-get :fp)
    ;; TODO seems that recursive call descriptor should go here too
    _ (stack-push (vm-codeofs (- p ep))) ; return offset
    _ (stack-push (vm-codeofs ep)) ; current entry point
    _ (stack-push (vm-int argc))
    _ (stack-push (vm-stack fp))
    sp (reg-get :sp)
    _ (reg-set :fp sp)
    _ (reg-set :ep func-offset)
    mh (get-method-header func-offset)
                                        ;            _ (m-when (check-argc mh arg_count))
    _ (m-seq (repeat (:local-variable-count mh) (stack-push (vm-nil))))
    _ (set-pc (+ func-offset (:code-offset mh)))]
   nil))

(defn property-accessor
  "Construct monadic function using locate-property-fn to search properties and
handle-property-fn to handle the results. locate-property-fn has signature of Metaclass
get-property or inherit-property. handle-property-fn accepts target-val
as (vm-obj), defining-obj as (vm-obj) ^int pid ^int prop-val ^int argc"
  [locate-property-fn handle-property-fn]

  (fn [target-val pid argc]

    {:pre [(number? pid)]}

    (trace "Accessing property" pid "of" target-val)
    (cond
     (vm-list? target-val) (do-vm
                            [lst (as-list target-val)
                             :let [tlst (t3chnique.metaclass.list/tads-list lst)]
                             [defining-obj prop-val] (locate-property-fn tlst pid argc)
                             r (handle-property-fn tlst defining-obj tlst pid prop-val argc)]
                            r)
     (vm-sstring? target-val) (abort "TODO Constant string property access")
     (vm-obj? target-val) (do-vm
                           [target (obj-retrieve (value target-val))
                            [defining-obj prop-val] (locate-property-fn target pid argc)
                            r (handle-property-fn target-val defining-obj target-val pid prop-val argc)]
                           r)
     (vm-nil?) (abort "VMERR_NIL_DEREF")
     :else (abort "VMERR_OBJ_VAL_REQD"))))

(declare say-value)

(defn eval-prop
  "Property handler which calls a method if appropriate or returns
property value directly."
  [target defining self pid prop-val argc]
  (trace "vm/eval-prop" self pid prop-val argc)
  (if prop-val
    (cond
     (not (vm-auto-eval? prop-val)) (return prop-val)
     (vm-dstring? prop-val) (m->>
                             (stack-push prop-val)
                             (say-value))
     (vm-codeofs? prop-val) (prepare-frame (vm-prop pid)
                                           target
                                           defining
                                           self
                                           (value prop-val)
                                           argc))
    (abort "not implemented propNotDefined")))

(defn data-only
  "Property handler which won't call methods but only returns data
items if available."
  [target defining self pid prop-val argc]
  (trace "vm/data-only" self pid prop-val argc)
  (if prop-val
    (cond
     (not (vm-auto-eval? prop-val)) (return prop-val)
     :else (abort "BAD_SPEC_EVAL"))))

(def generic-get-prop (property-accessor mc/get-property eval-prop))
(def generic-inherit-prop (property-accessor mc/inherit-property eval-prop))
(def generic-get-prop-data (property-accessor mc/get-property data-only))

(defn- say-value
  "Take the value from top of stack and say it."
  []
  (trace "say-value")
  (do-vm
   [self (get-stack-self)
    sm (get-say-method)
    sf (get-say-function)
    :cond [(and (valid? self) (valid? sm))
           [_ (generic-get-prop self sm 1)]
           
           (valid? sf)
           [_ (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) (value sf) 1)]

           :else
           [_ (abort "VMERR_SAY_IS_NOT_DEFINED")]]]
   nil))

(defop call 0x58 [:ubyte arg_count :uint4 func_offset]
  (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) func_offset arg_count))

(defop ptrcall 0x59 [:ubyte arg_count]
  (do-vm
   [val (stack-pop)
    r (cond
       (vm-funcptr? val) (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) (value val) arg_count)
       (vm-prop? val) (abort "todo as per ptrcallpropself")
       (vm-obj? val) (abort "todo ObjectCallProp")
       :else (abort "FUNCPTR_VAL_REQD"))]
   r))

(defop getprop 0x60 [:uint2 prop_id]
  (do-vm
    [obj (stack-pop)
     r (generic-get-prop obj prop_id 0)]
    r))

(defop callprop 0x61 [:ubyte arg_count :uint2 prop_id]
  (do-vm
   [obj (stack-pop)
    r (generic-get-prop obj prop_id arg_count)]
   r))

(defop ptrcallprop 0x62 [:ubyte arg_count]
  (do-vm
   [prop (stack-pop)
    target-val (stack-pop)
    r (generic-get-prop target-val (value prop) arg_count)]
   r))

(defop getpropself 0x63 [:uint2 prop_id]
  (do-vm
   [self (get-stack-self)
    r (generic-get-prop self prop_id 0)]
   r))

(defop callpropself 0x64 [:ubyte arg_count :uint2 prop_id]
  (do-vm
   [self (get-stack-self)
    r (generic-get-prop self prop_id arg_count)]
   r))

(defop ptrcallpropself 0x65 [:ubyte arg_count]
  (do-vm
   [self (get-stack-self)
    prop (stack-pop)
    r (generic-get-prop self (value prop) arg_count)]
   r))

(defop objgetprop 0x66 [:uint4 obj_id :uint2 prop_id]
  (generic-get-prop (vm-obj obj_id) prop_id 0))

(defop objcallprop 0x67 [:ubyte arg_count :uint4 obj_id :uint2 prop_id]
  (generic-get-prop (vm-obj obj_id) prop_id arg_count))

(defop getpropdata 0x68 [:uint2 prop_id]
  (do-vm
   [target-val (stack-pop)
    r (generic-get-prop-data target-val prop_id)]
   r))

(defop ptrgetpropdata 0x69 []
  (do-vm
   [prop (stack-pop)
    target-val (stack-pop)
    r (generic-get-prop-data target-val (value prop))]
   r))

(defop getproplcl1 0x6A [:ubyte local_number :uint2 prop_id]
  (do-vm
   [target-val (get-stack-local local_number)
    r (generic-get-prop target-val prop_id 0)]
   r))

(defop callproplcl1 0x6B [:ubyte arg_count :ubyte local_number :uint2 prop_id]
  (do-vm
   [target-val (get-stack-local local_number)
    r (generic-get-prop target-val prop_id arg_count)]
   r))

(defop getpropr0 0x6C [:uint2 prop_id]
  (do-vm
   [target-val (reg-get :r0)
    r (generic-get-prop target-val prop_id 0)]
   r))

(defop callpropr0 0x6D [:ubyte arg_count :uint2 prop_id]
  (do-vm
   [target-val (reg-get :r0)
    r (generic-get-prop target-val prop_id arg_count)]
   r))

(defop inherit 0x72 [:ubyte arg_count :uint2 prop_id]
  (do-vm
   [target-val (stack-pop)
    r (generic-inherit-prop target-val prop_id arg_count)]
   r))

(defop ptrinherit 0x73 [:ubyte arg_count]
  (do-vm
   [prop (stack-pop)
    target-val (stack-pop)
    r (generic-inherit-prop target-val (value prop) arg_count)]
   r))

(defop expinherit 0x74 [:ubyte arg_count :uint2 prop_id :uint4 obj_id]
  ; TODO expinherit
  )

(defop ptrexpinherit 0x75 [:ubyte arg_count :uint4 obj_id]
  ; TODO ptrexpinherit
  )

(defop varargc 0x76 []
  ; TODO varags
  )

(defop delegate 0x77 [:ubyte arg_count :uint2 prop_id]
  ; TODO delegation
  )

(defop ptrdelegate 0x78 [:ubyte arg_count]
  ; TODO delegation
  )

(defop swap2 0x7a []
  (with-stack [d c b a] [a b c d]))

(defop swapn 0x7b [:ubyte idx1 :ubyte idx2]
  (do-vm
   [sp (reg-get :sp)
    _ (stack-swap (- sp idx1) (- sp idx2))]
   nil))

;; A set of operations which retrieve an item from lower down the
;; stack and push it onto the stack

(defn- copy [reg offsetf]
  (do-vm
   [fp (reg-get reg)
    rv (stack-get (offsetf fp))
    _ (stack-push rv)]
   nil))

(defop getlcl1 0x80 [:ubyte local_number]
  (copy :fp (partial + local_number)))

(defop getlcl2 0x81 [:uint2 local_number]
  (copy :fp (partial + local_number)))

(defop getarg1 0x82 [:ubyte param_number]
  (copy :fp #(- % (+ FRAME-SIZE (inc param_number)))))

(defop getarg2 0x83 [:uint2 param_number]
  (copy :fp #(- % (+ FRAME-SIZE (inc param_number)))))

(defop getargn0 0x7C []
  (copy :fp #(- % (inc FRAME-SIZE))))

(defop getargn1 0x7D []
  (copy :fp #(- % (+ 2 FRAME-SIZE))))

(defop getargn2 0x7E []
  (copy :fp #(- % (+ 3 FRAME-SIZE))))

(defop getargn3 0x7F []
  (copy :fp #(- % (+ 4 FRAME-SIZE))))

(defop pushself 0x84 []
  (copy :fp #(- % (- FRAME-SIZE 3))))

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
  (do-vm
   [r0 (reg-get :r0)
    _ (stack-push r0)]
   nil))

; TODO debugger
(defop getdbargc 0x8C [])

(defop swap 0x8D []
  (with-stack [x y] [y x]))

; TODO context store / retrieve
(defop pushctxele 0x8E [:ubyte element])

(defop dup2 0x8F []
  (with-stack [a b] [a b a b]))

;; The switch instruction requires a custom parser, as the case
;; table is embedded in the op code.
;;
;; http://www.tads.org/t3doc/doc/techman/t3spec/opcode.htm#opc_switch 
(def switch-instruction-parser
  (domonad parse/byteparser-m
    [count (parse/uint2)
     cases (parse/times count
                        (parse/record :case-val (parse/data-holder)
                                      :case-branch (parse/int2)))
     default (parse/int2)]
    {:count count
     :cases cases
     :default default}))

(defn resolve-offsets
  "Throughout the instruction the program counter points just past the
opcode and jump jumps relative to the instruction, however the branch
offsets are relative to the bytes representing the offset so adjust
them to account for the difference."
  [{:keys [count cases] :as switch-instruction}]
  (->  switch-instruction
       (update-in [:cases]
                  #(map (fn [case index]
                          (assoc case :case-branch 
                                 (+ 1                 ; opcode
                                    2                 ; count
                                    (* 5 (inc index)) ; dataholder
                                    (* 2 index) ; previous offsets
                                    (:case-branch case))))
                        %
                        (range)))
       (update-in [:default] #(+ %
                                 1
                                 2
                                 (* 7 count)))))

(defop switch 0x90 [switch-instruction-parser switch-instruction]
  (let [inst (resolve-offsets switch-instruction)
        find-offset (fn [val]
                      (or (:case-branch (first (filter
                                                (fn [case] (vm-eq? val (:case-val case)))
                                                (:cases inst))))
                          (:default inst)))]
    (do-vm
     [val (stack-pop)
      pc (pc)
      _ (jump-from-ip (find-offset val))]
     (do pc nil))))

;; Various jump and branch operations

(defop jmp 0x91 [:int2 branch_offset]
  (jump-from-ip (inc branch_offset)))

(defn- jump-cond1
  [jumpif? branch_offset]
  (do-vm
   [v (stack-pop)
    _ (m-when (jumpif? v) (jump-from-ip (inc branch_offset)))]
   nil))

(defn- jump-cond2
  [jumpif? branch_offset]
  (do-vm
   [v2 (stack-pop)
    v1 (stack-pop)
    _ (m-when (jumpif? v1 v2) (jump-from-ip (inc branch_offset)))]
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
  (do-vm [v (stack-peek)
                 _ (if (not (vm-falsey? v))
                     (jump-from-ip (inc branch_offset))
                     (stack-pop))]
           nil))

(defop jsf 0x9B [:int2 branch_offset]
  (do-vm [v (stack-peek)
                 _ (if (vm-falsey? v)
                     (jump-from-ip (inc branch_offset))
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
  (do-vm [r0 (reg-get :r0)
          _ (m-when (not (vm-falsey? r0)) (jump-from-ip (inc branch_offset)))]
         nil))

(defop jr0f 0xA1 [:int2 branch_offset]
  (do-vm [r0 (reg-get :r0)
          _ (m-when (vm-falsey? r0) (jump-from-ip (inc branch_offset)))]
         nil))

(defop getspn 0xA6 [:ubyte index]
  (copy :sp #(- % (inc index))))

(defn- getlcl [i]
  (copy :fp (partial + i)))

(defop getlcln0 0xAA []
  (getlcl 0))

(defop getlcln1 0xAB []
  (getlcl 1))

(defop getlcln2 0xAC []
  (getlcl 2))

(defop getlcln3 0xAD []
  (getlcl 3))

(defop getlcln4 0xAE []
  (getlcl 4))

(defop getlcln5 0xAF []
  (getlcl 5))

(defop say 0xB0 [:uint4 offset]
  (do-vm
   [_ (stack-push (vm-sstring offset))
    _ (say-value)]
   nil))

(defn- bif
  "Call intrinsic function with index in set using argc arguments from stack. "
  [host set index argc]
  (do-vm
    [fnsd (fetch-val :fnsd)
     _ (bif/invoke-by-index host (nth fnsd set) index argc)]
    nil))

(defop builtin_a 0xB1 [:ubyte argc :ubyte func_index]
  (bif host 0 func_index argc))

(defop builtin_b 0xB2 [:ubyte argc :ubyte func_index]
  (bif host 1 func_index argc))

(defop builtin_c 0xB3 [:ubyte argc :ubyte func_index]
  (bif host 2 func_index argc))

(defop builtin_d 0xB4 [:ubyte argc :ubyte func_index]
  (bif host 3 func_index argc))

(defop builtin1 0xB5 [:ubyte argc :ubyte func_index :ubyte set_index]
  (bif host set_index func_index argc))

(defop builtin2 0xB6 [:ubyte argc :uint2 func_index :ubyte set_index]
  (bif host set_index func_index argc))

(defop callext 0xB7 []
  (abort "callext not implemented"))

; TODO implement
(defop throw 0xB8 []
  (do-vm
    [ep (reg-get :ep)
     ip (reg-get :ip)
     mh (get-method-header ep)
     et (:etable-offset mh)
     ex (get-exception-table et)
                                        ;            _ (filter #(<= (:first-offset %) (dec ip) (:last-offset)) ex)
     ]
    nil))

(defop sayval 0xB9 []
  (say-value))

(defn- apply-index [obj idx]
  {:pre [(vm-primitive? obj) (number? idx)]}
  (do-vm
   [sq (as-list obj)
    :if sq
    :then [_ (stack-push (nth sq (dec idx)))]
    :else [_ (abort "Op overload not implemented")]]
   nil))

(defop index 0xBA []
  (do-vm
   [idx (stack-pop)
    obj (stack-pop)
    _ (apply-index obj (value idx))]
   nil))

(defop idxlcl1int8 0xBB [:ubyte local_number :ubyte index_val]
  (do-vm
   [obj (getlcl local_number)
    _ (apply-index obj index_val)]
   nil))

(defop idxint8 0xBC [:ubyte index_val]
  (do-vm
   [obj (stack-pop)
    _ (apply-index obj index_val)]
   nil))

;; TODO byte code construction
(defop new1 0xC0 [:ubyte arg_count :ubyte metaclass_id]
  (do-vm
   [proto (m-apply #(mc/prototype % metaclass_id))
    obj (mc/load-from-stack proto arg_count)
    :let [obj (assoc obj :metaclass metaclass_id)]
    id (new-obj-id)
    _ (obj-store id obj)
    _ (return (vm-obj id))]
   nil))

(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id]
  )

;; TODO
(defop trnew1 0xC2 [:ubyte arg_count :ubyte metaclass_id])

;; TODO
(defop trnew2 0xC3 [:uint2 arg_count :uint2 metaclass_id])

(defn- setlcl [i v]
  (do-vm [fp (reg-get :fp)
          _ (stack-set (+ fp i) v)]
         nil))

(defn- updlcl [i f]
  (do-vm [fp (reg-get :fp)
          v (stack-get (+ fp i))
          _ (stack-set (+ fp i) (f v))]
         nil))

(defn- updlcl-m [i mv]
  (do-vm
   [fp (reg-get :fp)
    v (stack-get (+ fp i))
    v+ (mv v)
    _ (stack-set (+ fp i) v+)]
   nil))

(defop inclcl 0xD0 [:uint2 local_number]
  (updlcl local_number vm-inc))

; TODO
(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id])

(defop declcl 0xD1 [:uint2 local_number]
  (updlcl local_number vm-dec))

(defop addilcl1 0xD2 [:ubyte local_number :sbyte val]
  (updlcl-m local_number (partial compute-sum val)))

(defop addilcl4 0xD3 [:uint2 local_number :int4 val]
  (updlcl-m local_number (partial compute-sum val)))

(defop addtolcl 0xD4 [:uint2 local_number]
  (do-vm [v (stack-pop)
          _ (updlcl-m local_number #(compute-sum % v))]
         nil))

(defop subfromlcl 0xD5 [:uint2 local_number]
  (do-vm [v (stack-pop)
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
  (do-vm [v (stack-pop)
          _ (setlcl local_number v)]
         nil))

(defop setlcl2 0xE1 [:uint2 local_number]
  (do-vm [v (stack-pop)
          _ (setlcl local_number v)]
         nil))

(defop setarg1 0xE2 [:ubyte arg_number]
  (do-vm [fp (reg-get :fp)
          v (stack-pop)
          _ (stack-set (- fp (+ 9 arg_number)) v)]
         nil))

(defop setarg2 0xE3 [:uint2 arg_number]
  (do-vm [fp (reg-get :fp)
          v (stack-pop)
          _ (stack-set (- fp (+ 9 arg_number)) v)]
         nil))

;TODO setind
(defop setind 0xE4 [])

(defn- set-property
  "Updates object store to contain modified version of object
with property set as specified. oid, pid numbers. val primitive."
  [oid pid val]
  (do-vm
   [object (obj-retrieve oid)
    new-object (mc/set-property object pid val)
    _ (obj-store oid new-object)]
   nil))

(defop setprop 0xE5 [:uint2 prop_id]
  (do-vm
   [obj-id (stack-pop)
    new-val (stack-pop)
    ret (set-property (value (vm-obj-check obj-id "OBJ_VAL_REQUIRED")) prop_id new-val)]
   ret))

(defop ptrsetprop 0xE6 []
  (do-vm
   [prop-id (stack-pop)
    obj (stack-pop)
    new-val (stack-pop)
    ret (set-property (value obj) (value prop-id) new-val)]
   ret))

(defop setpropself 0xE7 [:uint2 prop_id]
  (do-vm
   [new-val (stack-pop)
    self (get-stack-self)
    ret (set-property (value self) prop_id new-val)]
   ret))

(defop objsetprop 0xE8 [:uint4 obj :uint2 prop_id]
  (do-vm
   [new-val (stack-pop)
    ret (set-property obj prop_id new-val)]
   ret))

;TODO setdblcl
(defop setdblcl 0xE9 [:uint2 local_number])

;TODO setdbarg
(defop setdbarg 0xEA [:uint2 param_number])

;TODO setself
(defop setself 0xEB [])

;TODO loadctx
(defop loadctx 0xEC [])

;TODO storectx
(defop storectx 0xED [])

(defop setlcl1r0 0xEE [:ubyte local_number]
  (do-vm [v (reg-get :r0)
          _ (setlcl local_number v)]
         nil))

(defop setindlcl1i8 0xEF [:ubyte local_number :ubyte index_val]
  )

;; TODO debugger
(defop bp 0xF1 [])

(defop nop 0xF2 [])

;; control

(defn enter
  "Set up vm at initial entry point."
  [host]
  (do-vm
   [entp (fetch-val :entry-point-offset)
    _ (op-pushlst host 0)
    _ (op-call host 1 entp)
    _ (commit-pc)]
   nil))

(defn step
  "Execute the op code referenced by the ip register."
  [host]
  (do-vm
   [[op args len] (parse-op-at-ip)
    ip (reg-get :ip)
    _ (set-pc (+ ip len))
    ret (apply (:run-fn op) (cons host (vals args)))
    _ (commit-pc)]
   ret))

(defn run-state
  "Run until an error occurs. Explicitly in the state monad!"
  [host]
  (fn [s]
    (loop [state s]
      (assert state)
      (let [[newr new-state] ((step host) state)]
        (assert new-state)
        (if-let [e (or newr (:exc new-state))]
          [e new-state]
          (recur new-state))))))
