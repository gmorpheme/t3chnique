(ns ^{:doc "Main VM implementation including state and ops. Depends on metaclass
and function set protocols."}
    t3chnique.vm
  (:use [t3chnique.primitive])
  (:require [monads.core :as m :refer [mdo return modify get-state put-state >> >>=]]
            [monads.util :as u :refer [sequence-m]]
            [monads.state :refer [run-state]]
            [t3chnique.parse :as parse]
            [t3chnique.intrinsics :as bif]
            [t3chnique.metaclass :as mc]
            [clojure.tools.logging :refer [spy debug info trace tracef error]])
  (:import [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

;;; The VM has an awkward circular relationship with string and list
;;; metaclasses. In general the core vm should depend only on the
;;; metaclass protocols and individual metaclass implementations are
;;; then layered on top. However TADS3 also has for "constant" strings
;;; and lists as well which share implementation with the string and
;;; list metaclasses and the VM needs a more intimate relationship with
;;; these than just using the metaclass API.
;;;
;;; This is a workaround for the circular dependencies until a better
;;; solution can be found.
(do (in-ns 't3chnique.metaclass.string)
    (clojure.core/declare add-to-str)
    (clojure.core/declare create)
    (in-ns 't3chnique.metaclass.list)
    (clojure.core/declare tads-list)
    (in-ns 't3chnique.vm))

;;; The representation of VM state is simply a map. Registers are top level
;;; keys, various image blocks are represented in varying degrees of structure
;;; in other keys.
(defn vm-state
  "Initialise a blank VM state map."
  []
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

;;;;
;;;; Image loading
;;;;
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

;; Assoc the metaclasses into the state map and link up the records
;; with their implementations.
(defmethod load-image-block "MCLD" [s b]
  (assoc s :mcld (mc/wire-up-metaclasses (:entries b))))

;; Assoc the objects into the state map and if they're intrinsic class
;; objects, link them up to the related metaclass implementations.
(defmethod load-image-block "OBJS" [s b]
  (let [objs (mc/read-object-block (:mcld s) b)
        _ (trace "loaded objects: " (keys objs))
        mcld (mc/process-intrinsic-class-objects (:mcld s) objs)]
    (assoc
        (merge-with merge s {:objs objs})
      :mcld mcld)))

;; End of image file, so initialise up any counters etc.
(defmethod load-image-block "EOF " [s b]
  (assoc s :next-oid (inc (apply max (keys (:objs s))))))

(defmethod load-image-block "MRES" [s b]
  s)

(defn vm-from-image
  "Load a VM image file and translate into a state map."
  [bs]
  (info "===*** Loading new VM image ***===")
  (reduce load-image-block (vm-state) bs))

(defn hydrate-state
  "VM state has transient items pruned when dumped. Hydrate to set these
   up again after load."
  [s]
  (->> s (mc/hydrate-mcld)))

;;; Most of the core VM is implemented as a set of opcodes, each of which
;;; defines a value in the VM state monad.

;;; We define op structure, definition macro and opcode table for lookup
;;; by code.
(defrecord OpCode [code mnemonic parse-spec run-fn])

;; TODO - no need for this to be an atom, use var and alter bindings
;; as ops are defined.
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
        op-fn-name (symbol (str "op-" op))]
    `(do
       (defn ^{:opcode ~cd} ~op-fn-name ~param-syms
         (debug '~op ~(vec (rest param-syms)))
         (runop (fn [] ~@exprs)))

       (swap! table assoc ~cd (OpCode. ~cd '~op ~spec ~(symbol (str "op-" op)))))))

(defmacro with-stack
  "Bind symbols to values popped of top of stack and push result of exprs back on"
  [syms exprs]
  (let [binds (vec (reverse syms))
        pops (vec (repeat (count syms) 'stack-pop))
        pushes (for [e exprs] (list 'stack-push e))]
    `(mdo
      ~binds ~'<- (u/sequence-m ~pops)
      ~@pushes)))

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

(def parse-op
  "Parser for reading op code and args from offset into code pool."
  (m/mdo
   opcode <- parse/ubyte
   let op = (or (@table opcode) (throw (RuntimeException. (format "Unknown opcode: 0x%x" opcode))))
   args <- (parse/spec (:parse-spec op))
   (m/return [op args])))

(defn read-op
  "Read the operation in the supplied buffer position.
Return opcode map, args map and byte length of compete instruction."
  [[b i]]
  (let [[[op args] [b' i']] (parse/run-parse parse-op [b i])]
    [op args (- i' i)]))

;; We leave :ip pointing at the current instruction during processing.
;; Should opcodes need to jump, they set :pc instead and this will be
;; committed to :ip at the end of the step.

(defn set-val [key val]
  (modify #(assoc % key val)))

(defn get-val [key]
  (mdo
   st <- get-state
   (return (key st))))

(defn rm-val [key]
  (modify #(dissoc % key)))

(defn update-val [key f]
  (modify #(assoc % key (f (key %)))))

(defn set-pc [val]
  (modify #(assoc % :pc val)))

(defn pc []
  (mdo
   pc <- (get-val :pc)
   (if pc
     (return pc)
     (throw (ex-info "No program counter - op incorrectly started" {})))))

(defn commit-pc
  "Commit the in-progress program counter back to ip."
  []
  (mdo
   pc <- (pc)
   (set-val :ip pc)
   (rm-val :pc)))

(def parse-op-at-ip
  "Action to parse operation at current instruction pointer."
  (>>= get-state
       #(return (read-op (offset %)))))

(def get-say-method (get-val :say-method))
(def set-say-method (partial set-val :say-method))
(def get-say-function (get-val :say-function))
(def set-say-function (partial set-val :say-function))

;; Stack actions

(defn stack-push
  "Action to push value onto the VM stack."
  [val]
  {:pre [(vm-primitive? val)]}
  (>> (update-val :stack #(conj % val))
      (update-val :sp inc)))

(def stack-peek
  "Action to return value at top of stack"
  (get-val (comp last :stack)))

(def stack-pop
  (mdo
   top <- stack-peek
   (update-val :stack pop)
   (update-val :sp dec)
   (return top)))

(defn stack-set [idx val]
  {:pre [(integer? idx) (vm-primitive? val)]}
  (update-val :stack #(assoc % idx val)))

(defn stack-get [idx]
  {:pre [(integer? idx)]}
  (>>= (get-val :stack) #(return (nth % idx))))

(defn stack-update [idx f]
  {:pre [(integer? idx) (fn? f)]}
  (update-val :stack #(update-in % [idx] f)))

(defn stack-swap [i j]
  {:pre [(integer? i) (integer? j)]}
  (update-val :stack (fn [stack] (-> stack
                                    (assoc i (stack j))
                                    (assoc j (stack i))))))

(defn jump-from-ip
  "Jump to an offset from the instruction byte code (still stored in :ip) by
setting the program counter (pc) - this will be committed to ip once the
instruction is complete."
  [offset-from-ip]
  {:pre [(integer? offset-from-ip)]}
  (>>= (get-val :ip) #(set-pc (+ % offset-from-ip))))

(def reg-get get-val)

(defn reg-set [reg val]
  {:pre [(keyword? reg)]}
  (set-val reg val))

(def vm-return (partial reg-set :r0))

(defn symbol-value [sym]
  {:pre [(string? sym)]
   :post [vm-primitive?]}
  (>>=
   (get-val :symd)
   #(return (get % sym))))

(defn new-obj-id []
  {:post [integer?]}
  (mdo
   oid <- (get-val :next-oid)
   (update-val :next-oid inc)
   (return oid)))

(defn obj-store [oid o]
  {:pre [(integer? oid)]}
  (update-val :objs #(assoc % oid o)))

(defn obj-intern [o]
  {:pre [(record? o)]
   :post [vm-primitive?]}
  (mdo
   oid <- (new-obj-id)
   (obj-store oid o)
   (return (vm-obj oid))))

(defn obj-retrieve [oid]
  {:pre [(number? oid)]
   :post [record?]}
  (trace "obj-retrieve" oid)
  (get-val #(get-in % [:objs oid])))

(defn get-method-header
  "Read method header at specified offset."
  [ptr]
  {:pre [(integer? ptr)]}
  (trace "get-method-header" ptr)
  (>>=
   get-state
   #(return (parse/parse-at (parse/method-header (:method-header-size %)) (offset % ptr)))))

(defn get-exception-table
  "Read exception table at specified offset."
  [ptr]
  {:pre [(integer? ptr)]}
  (>>=
   get-state
   #(return (parse/parse-at (parse/exception-table) (offset % ptr)))))

(def tick
  "Action to update sequence number in state."
  (update-val :sequence inc))

(defn runop
  [op]
  (mdo
   exception <- (op)
   tick
   (return exception)))

;; Operations on primitives / op overloads

(defn load-string-constant
  "Read a string (prefixed utf-8) from the constant pool."
  [state address]
  {:pre [(integer? address)]}
  (let [[buf idx] (const-offset state address)]
    (parse/parse-at parse/prefixed-utf8 buf idx)))

(defn as-string
  "Get actual string from sstring or object string."
  [v]
  {:pre [(vm-primitive? v)]}
  (cond
   (vm-sstring? v) (get-val #(load-string-constant % (value v)))
   (vm-obj? v) (>>= (obj-retrieve (value v)) #(return (mc/get-as-string %)))))

(defn load-list-constant
  "Read a list (prefixed) from the constant pool."
  [state address]
  {:pre [(integer? address)]}
  (let [[buf idx] (const-offset state address)]
    (parse/parse-at parse/lst buf idx)))

(defn as-list
  "Get seq from list or object list"
  [v]
  {:pre [(vm-primitive? v)]
   :post [#(or (record? %) (nil? %))]}
  (cond
   (vm-list? v) (get-val #(load-list-constant % (value v)))
   (vm-obj? v) (>>= (obj-retrieve (value v)) #(return (mc/get-as-seq %)))
   :else nil))

;; TODO should this be in string metaclass?
(defn convert-to-string 
  "Return internal string representation (not vm-sstring)."
  [v]
  {:pre [(vm-primitive? v)]}
  (trace "convert-to-string" v)
  (cond

   (or (vm-sstring? v)
       (vm-dstring? v)) (get-val #(load-string-constant % (value v)))
   
   (vm-obj? v) (mdo
                obj <- (obj-retrieve (value v))
                (return (mc/get-as-string obj))) ;TODO metaclass cast_to_string

   (vm-int? v) (return (str (value v)))

   (vm-nil? v) (return "nil")
   
   (vm-true? v) (return "true")
   
   :else (throw (ex-info "TODO other string conversions" {:value (mnemonise v)}))))

;; TODO non-numerics
(defn- vm-lift2
  "Lift an operation to apply to VM primitives"
  ([op retf]
     (fn [a b]
       {:pre [(vm-primitive? a) (vm-primitive? b)]}
       (cond
        (vm-int? a) (if (vm-int? b)
                      (retf (op (value a) (value b)))
                      (throw (ex-info "Operands not numeric" {:code :NUM_VAL_REQD :left a :right b})))
        :else (throw (ex-info "NUM_VAL_REQD" {:code :NUM_VAL_REQD :left a :right b})))))
  ([op]
     (vm-lift2 op identity)))

(defn- vm-lift1
  "Life an operator to apply to VM primitive."
  ([op retf]
     (fn [a]
       {:pre [(vm-primitive? a)]}
       (cond
        (vm-int? a) (retf (op (value a)))
        :else (throw (ex-info "Numeric arg required" {:code :NUM_VAL_REQD :value a}))))))

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
(def vm-+ (vm-lift2 + vm-int))
(def vm-- (vm-lift2 - vm-int))
(def vm-* (vm-lift2 * vm-int))
(def vm-div (vm-lift2 / vm-int)) ; TODO div by zero
(def vm-mod (vm-lift2 mod vm-int)) ; TODO div by zero
(def vm-<< (vm-lift2 bit-shift-left vm-int))
(def vm->> (vm-lift2 bit-shift-right vm-int))
(def vm-inc (vm-lift1 inc vm-int))
(def vm-dec (vm-lift1 dec vm-int))
(def vm->>> (vm-lift2 unsigned-bit-shift-right vm-int))
(def vm-xor (vm-lift2 bit-xor vm-int))

(defn vm-falsey? [val]
  {:pre [(vm-primitive? val)]}
  (or (vm-nil? val)
      (and (vm-int? val) (vm-zero? val))))

(defn vm-truthy? [val]
  (not (vm-falsey? val)))

; TODO non numeric
(defn vm-eq? [a b]
  {:pre [(vm-primitive? a) (vm-primitive? b)]}
  (cond
   (vm-nil? a) (vm-nil? b)
   (vm-true? a) (vm-true? b)
   (vm-int? a) (and (vm-int? b) (= (value a) (value b)))
   (vm-enum? a) (and (vm-enum? b (= (value a) (value b))))
   :else (throw (ex-info "TODO: vm-eq? not implemented for type" {:left a :right b}))))

;;;;
;;;; Op codes
;;;;
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
  (obj-intern (t3chnique.metaclass.string/create string_bytes)))

(defop pushparlst 0x0D [:ubyte fixed_arg_count]
  (throw (ex-info "TODO: implement pushparlst" {})))

(defop makelstpar 0x0E []
  (throw (ex-info "TODO: implement makelstpar" {})))

(defop pushenum 0x0F [:int4 val]
  (stack-push (vm-enum val)))

(defop pushbifptr 0x10 [:uint2 function_index :uint2 set_index]
  (throw (ex-info "TODO: implement pushbifptr" {})))

(defop neg 0x20 []
  (with-stack [val]
    [(if (vm-int? val) (vm-int (- (value val))) (throw (ex-info "TODO: neg for non-numerics" {:value val})))]))

(defn vm-bnot [val]
  {:pre [(vm-primitive? val)]}
  (if (vm-int? val)
    (vm-int (bit-and 0xffffffff (bit-not (value val))))
    (throw (ex-info "TODO: bnot non-numberics" {:value val}))))

(defn- stack-op1 [op]
  (with-stack [val] [(op val)]))

(defn- stack-op2 [op]
  (with-stack [a b] [(op a b)]))

(defop bnot 0x21 []
  (stack-op1 vm-bnot))

(defn- compute-sum [a b]
  {:pre [(vm-primitive? a) (vm-primitive? b)]
   :post [vm-primitive?]}
  (trace "Compute sum " a b)
  (cond
   (vm-int? a) (return (vm-+ a b))
   (vm-sstring? a) (t3chnique.metaclass.string/add-to-str a b)
   (vm-obj? a) (t3chnique.metaclass.string/add-to-str a b)
   :else (throw (ex-info "BAD_TYPE_ADD" {:code :BAD_TYPE_ADD :left a :right b}))))

(defop add 0x22 []
  (mdo
   [r l] <- (sequence-m (repeat 2 stack-pop))
   result <- (compute-sum l r)
   (stack-push result)))

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

(def unwind
  "Unwind stack on function exit and restore calling frame."
  (mdo
   sp <- (reg-get :sp)
   fp <- (reg-get :fp)
   (sequence-m (repeat (- sp fp) stack-pop))
   fp <- stack-pop
   ac <- stack-pop
   of <- stack-pop
   ep <- stack-pop
   rc <- stack-pop
   (sequence-m (repeat (+ 6 (value ac)) stack-pop))
   (reg-set :fp (value fp))
   (reg-set :ep (value ep))
   (set-pc (+ (value ep) (value of)))))

(defop retval 0x50 []
  (>> (>>= stack-pop vm-return) unwind))

(defop retnil 0x51 []
  (>> (vm-return (vm-nil)) unwind))

(defop rettrue 0x52 []
  (>> (vm-return (vm-true)) unwind))

(defop ret 0x54 []
  unwind)

(defop namedargptr 0x56 [:ubyte named_arg_count :uint2 table_offset]
  (throw (ex-info "TODO: implement namedargptr" {})))

(defop namedargtab 0x57 [:named-arg-args args]
  (throw (ex-info "TODO: implement namedargtab" {})))

(def FRAME-SIZE 11)

(defn- get-stack-self []
  (mdo
   fp <- (reg-get :fp)
   self <- (stack-get (- fp (- FRAME-SIZE 3)))
   (return (if (vm-obj? self) self (vm-obj)))))

(defn- get-stack-local [i]
  (mdo
   fp <- (reg-get :fp)
   target <- (stack-get (+ fp i))
   (return target)))

;;;
;;; Function / property calling
;;; 

;;
;; Recursive calls into the VM are supported; calling stack contains
;; a record of how the recursive call was initiated so as to restore
;; the calling context in the calling VM.

(defrecord RecursiveCallContext [bif self idx argc argp caller-addr])

(defn recurse-from-bif [funcset idx argc]
  (RecursiveCallContext. funcset nil idx argc nil nil))

(defn recurse-from-intrinsic-class [self idx argc]
  (RecursiveCallContext. nil self idx argc nil nil))

(defn prepare-frame
  "Prepare stack frame for call. target-pid is (vm-prop), objs are
 (vm-obj) or (vm-nil). Func offset and argc are raw int. rc is 
instance of RecursiveCallContext."
  [target-pid target-obj defining-obj self-obj func-offset argc rc]
  {:pre [(vm-prop? target-pid)
         (vm-obj-or-nil? target-obj)
         (vm-obj-or-nil? defining-obj)
         (vm-obj-or-nil? self-obj)
         (number? func-offset)
         (number? argc)]}
  (mdo
   ;; invocation frame
   (stack-push target-pid) 
   (stack-push target-obj) ; object whose method is called
   (stack-push defining-obj) ; defining obj (may be superclass of target)
   (stack-push self-obj) ; self (method may belong to delegate)
   (stack-push (vm-nil)) ; TODO: "invokee"
   ep <- (reg-get :ep)
   p <- (pc)
   fp <- (reg-get :fp)
   ;; do_call
   (stack-push (vm-nil)) ; TODO: "stack frame references"
   ;; TODO seems that recursive call descriptor should go here too
   (stack-push (vm-codeptr rc))
   (stack-push (vm-codeofs (- p ep))) ; return offset
   (stack-push (vm-codeofs ep)) ; current entry point should be codeptr
   (stack-push (vm-int argc))
   (stack-push (vm-stack fp))
   sp <- (reg-get :sp)
   (reg-set :fp sp)
   (reg-set :ep func-offset)
   mh <- (get-method-header func-offset)
   (sequence-m (repeat (:local-variable-count mh) (stack-push (vm-nil))))
   (set-pc (+ func-offset (:code-offset mh)))))

(defn property-accessor
  "Construct monadic function using locate-property-fn to search properties and
handle-property-fn to handle the results. locate-property-fn has signature of Metaclass
get-property or inherit-property. handle-property-fn accepts target-val
as (vm-obj), defining-obj as (vm-obj) ^int pid ^int prop-val ^int argc"
  [locate-property-fn handle-property-fn]

  (fn [target-val pid argc]
    {:pre [(vm-primitive? target-val) (integer? pid) (integer? argc)]}
    (trace "Access property" pid "of" (mnemonise target-val))
    (mdo
     object <- (cond
                (vm-list? target-val) (>>= (as-list target-val) #(return (t3chnique.metaclass.list/tads-list %)))
                (vm-sstring? target-val) (throw (ex-info "TODO: Constant string property access" {}))
                (vm-obj? target-val) (obj-retrieve (value target-val))
                (vm-nil? target-val) (throw (ex-info "Nil dereference" {:code :VMERR_NIL_DEREF}))
                :else (throw (ex-info "Object value required" {:code :VMERR_OBJ_VAL_REQD})))
     [defining-obj prop-val] <- (locate-property-fn object pid argc)
     (handle-property-fn target-val defining-obj target-val pid prop-val argc))))

(declare say-value)

(defn eval-prop
  "Property handler which calls a method if appropriate or returns
property value directly."
  [target defining self pid prop-val argc]
  (trace "eval-prop" "self: " (mnemonise self) " property: " pid " prop-val: " (mnemonise prop-val) " argc: " argc)

  (if prop-val                          ; locator fns return nil if
                                        ; not found
    (cond
     (not (vm-auto-eval? prop-val)) (vm-return prop-val)
     (vm-dstring? prop-val) (>> (stack-push prop-val) (say-value))
     (vm-codeofs? prop-val) (prepare-frame (vm-prop pid)
                                           target
                                           defining
                                           self
                                           (value prop-val)
                                           argc
                                           nil)
     :else (throw (ex-info "TODO: unexpected property value type" {:property prop-val})))
    (throw (ex-info "TODO: implement propNotDefined" {:property prop-val}))))

(defn data-only
  "Property handler which won't call methods but only returns data
items if available."
  [target defining self pid prop-val argc]
  (trace "vm/data-only" self pid prop-val argc)
  (if (vm-truthy? prop-val)
    (cond
     (not (vm-auto-eval? prop-val)) (vm-return prop-val)
     :else (throw (ex-info "BAD_SPEC_EVAL" {:code :BAD_SPEC_EVAL})))))

(def generic-get-prop (property-accessor mc/get-property eval-prop))
(def generic-inherit-prop (property-accessor mc/inherit-property eval-prop))
(def generic-get-prop-data (property-accessor mc/get-property data-only))

(defn- say-value
  "Take the value from top of stack and say it."
  []
  (mdo
   self <- (get-stack-self)
   sm <- get-say-method
   sf <- get-say-function
   (cond
    (and (valid? self) (valid? sm)) (generic-get-prop self sm 1)
    (valid? sf) (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) (value sf) 1 nil)
    :else (throw (ex-info "No say method or function defined" {:code :VMERR_SAY_IS_NOT_DEFINED})))))

(defop call 0x58 [:ubyte arg_count :uint4 func_offset]
  (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) func_offset arg_count nil))

(defop ptrcall 0x59 [:ubyte arg_count]
  (mdo
   val <- stack-pop
   (cond
    (vm-funcptr? val) (prepare-frame (vm-prop 0) (vm-nil) (vm-nil) (vm-nil) (value val) arg_count nil)
    (vm-prop? val) (throw (ex-info "TODO: implement as per ptrcallpropself" {}))
    (vm-obj? val) (throw (ex-info "TODO: implement ObjectCallProp" {}))
    :else (throw (ex-info "ptrcall requires function pointer value" {:code :FUNCPTR_VAL_REQD})))))

(defop getprop 0x60 [:uint2 prop_id]
  (>>= stack-pop #(generic-get-prop % prop_id 0)))

(defop callprop 0x61 [:ubyte arg_count :uint2 prop_id]
  (>>= stack-pop #(generic-get-prop % prop_id arg_count)))

(defop ptrcallprop 0x62 [:ubyte arg_count]
  (mdo
   prop <- stack-pop
   target-val <- stack-pop
   (generic-get-prop target-val (value prop) arg_count)))

(defop getpropself 0x63 [:uint2 prop_id]
  (mdo
   self <- (get-stack-self)
   (generic-get-prop self prop_id 0)))

(defop callpropself 0x64 [:ubyte arg_count :uint2 prop_id]
  (mdo
   self <- (get-stack-self)
   (generic-get-prop self prop_id arg_count)))

(defop ptrcallpropself 0x65 [:ubyte arg_count]
  (mdo
   self <- (get-stack-self)
   prop <- stack-pop
   (generic-get-prop self (value prop) arg_count)))

(defop objgetprop 0x66 [:uint4 obj_id :uint2 prop_id]
  (generic-get-prop (vm-obj obj_id) prop_id 0))

(defop objcallprop 0x67 [:ubyte arg_count :uint4 obj_id :uint2 prop_id]
  (generic-get-prop (vm-obj obj_id) prop_id arg_count))

(defop getpropdata 0x68 [:uint2 prop_id]
  (>>= stack-pop #(generic-get-prop-data % prop_id)))

(defop ptrgetpropdata 0x69 []
  (mdo
   prop <- stack-pop
   target-val <- stack-pop
   (generic-get-prop-data target-val (value prop))))

(defop getproplcl1 0x6A [:ubyte local_number :uint2 prop_id]
  (>>= (get-stack-local local_number) #(generic-get-prop % prop_id 0)))

(defop callproplcl1 0x6B [:ubyte arg_count :ubyte local_number :uint2 prop_id]
  (>>= (get-stack-local local_number) #(generic-get-prop % prop_id arg_count)))

(defop getpropr0 0x6C [:uint2 prop_id]
  (>>= (reg-get :r0) #(generic-get-prop % prop_id 0)))

(defop callpropr0 0x6D [:ubyte arg_count :uint2 prop_id]
  (>>= (reg-get :r0) #(generic-get-prop % prop_id arg_count)))

(defop inherit 0x72 [:ubyte arg_count :uint2 prop_id]
  (>>= stack-pop #(generic-inherit-prop % prop_id arg_count)))

(defop ptrinherit 0x73 [:ubyte arg_count]
  (mdo
   prop <- stack-pop
   target-val <- stack-pop
   (generic-inherit-prop target-val (value prop) arg_count)))

(defop expinherit 0x74 [:ubyte arg_count :uint2 prop_id :uint4 obj_id]
  (throw (ex-info "TODO: expinherit" {})))

(defop ptrexpinherit 0x75 [:ubyte arg_count :uint4 obj_id]
  (throw (ex-info "TODO: ptrexpinherit" {})))

(defop varargc 0x76 []
  (throw (ex-info "TODO: varags" {})))

(defop delegate 0x77 [:ubyte arg_count :uint2 prop_id]
  (throw (ex-info "TODO: delegation" {})))

(defop ptrdelegate 0x78 [:ubyte arg_count]
  (throw (ex-info "TODO: delegation" {})))

(defop swap2 0x7a []
  (with-stack [d c b a] [a b c d]))

(defop swapn 0x7b [:ubyte idx1 :ubyte idx2]
  (>>= (reg-get :sp) #(stack-swap (- % idx1) (- % idx2))))

;; A set of operations which retrieve an item from lower down the
;; stack and push it onto the stack

(defn- copy [reg offsetf]
  (mdo
   fp <- (reg-get reg)
   (>>= (stack-get (offsetf fp)) stack-push)))

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

(defop getdblcl 0x85 [:uint2 local_number]
  (throw (ex-info "TODO: implement getdblcl" {})))

(defop getdbarg 0x86 [:uint2 param_number]
  (throw (ex-info "TODO: implement getdbarg" {})))

(defop getargc 0x87 []
  (copy :fp #(- % 2)))

(defop dup 0x88 []
  (with-stack [x] [x x]))

(defop disc 0x89 []
  stack-pop)

(defop disc1 0x89 [:ubyte count]
  (sequence-m (repeat count stack-pop)))

(defop getr0 0x8B []
  (>>= (reg-get :r0) stack-push))

(defop getdbargc 0x8C []
  (throw (ex-info "TODO: implement getdbargc" {})))

(defop swap 0x8D []
  (with-stack [x y] [y x]))

(defop pushctxele 0x8E [:ubyte element]
  (throw (ex-info "TODO:implement pushctxele" {})))

(defop dup2 0x8F []
  (with-stack [a b] [a b a b]))

;; The switch instruction requires a custom parser, as the case
;; table is embedded in the op code.
;;
;; http://www.tads.org/t3doc/doc/techman/t3spec/opcode.htm#opc_switch 
(def switch-instruction-parser
  (m/mdo
   count <- parse/uint2
   cases <- (parse/times count
                         (parse/record :case-val parse/data-holder
                                       :case-branch parse/int2))
   default <- parse/uint2
   (m/return {:count count :cases cases :default default})))

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
    (mdo
     val <- stack-pop
     pc <- (pc)
     (jump-from-ip (find-offset val)))))

;;;
;;; Various jump and branch operations
;;;
(defop jmp 0x91 [:int2 branch_offset]
  (jump-from-ip (inc branch_offset)))

(defn- jump-cond1
  [jumpif? branch_offset]
  (mdo
   v <- stack-pop
   (if (jumpif? v)
     (jump-from-ip (inc branch_offset))
     (return nil))))

(defn- jump-cond2
  [jumpif? branch_offset]
  (mdo
   v2 <- stack-pop
   v1 <- stack-pop
   (if (jumpif? v1 v2)
     (jump-from-ip (inc branch_offset))
     (return nil))))

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
  (mdo
   v <- stack-peek
   (if (vm-truthy? v)
       (jump-from-ip (inc branch_offset))
       stack-pop)))

(defop jsf 0x9B [:int2 branch_offset]
  (mdo
   v <- stack-peek
   (if (vm-falsey? v)
       (jump-from-ip (inc branch_offset))
       stack-pop)))

(defop ljsr 0x9C [:int2 branch_offset]
  (throw (ex-info "TODO: implement ljsr" {})))

(defop lret 0x9D [:int2 local_variable_number]
  (throw (ex-info "TODO: implement lret" {})))

(defop jnil 0x9E [:int2 branch_offset]
  (jump-cond1 vm-nil? branch_offset))

(defop jnotnil 0x9F [:int2 branch_offset]
  (jump-cond1 (complement vm-nil?) branch_offset))

(defop jr0t 0xA0 [:int2 branch_offset]
  (mdo
   r0 <- (reg-get :r0)
   (if (vm-truthy? r0)
     (jump-from-ip (inc branch_offset))
     (return nil))))

(defop jr0f 0xA1 [:int2 branch_offset]
  (mdo
   r0 <- (reg-get :r0)
   (if (vm-falsey? r0)
     (jump-from-ip (inc branch_offset))
     (return nil))))

(defn run-intrinsic-method
  "Most intrinsic methods are obj -> [ret obj]. This wraps to update in place."
  [object method]
  {:pre [(vm-obj? object)]}
  (let [oid (value object)]
    (mdo
     instance <- (obj-retrieve oid)
     [ret new-instance] <- (method instance)
     (obj-store oid new-instance)
     (return ret))))

(defn- getlcl [i]
  (copy :fp (partial + i)))

(defop iternext 0xA2 [:uint2 local_number :int2 offset]
  (mdo
   iterator <- (getlcl local_number)
   (if (vm-obj? iterator)
     (>>= (run-intrinsic-method iterator mc/iter-next) stack-push)
     (jump-from-ip (+ 5 offset)))))

;; #define OPC_ITERNEXT     0xA2                              /* iterator next */
;; #define OPC_GETSETLCL1R0 0xA3 /* set local from R0 and leave value on stack */
;; #define OPC_GETSETLCL1   0xA4         /* set local and leave value on stack */
;; #define OPC_DUPR0        0xA5                              /* push R0 twice */

(defop getspn 0xA6 [:ubyte index]
  (copy :sp #(- % (inc index))))

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
  (>>
   (stack-push (vm-sstring offset))
   (say-value)))

(defn- bif
  "Call intrinsic function with index in set using argc arguments from stack. "
  [host set index argc]
  (>>=
    (get-val :fnsd)
    #(bif/invoke-by-index host (nth % set) index argc)))

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
  (throw (ex-info "callext not implemented" {})))

(defop throw 0xB8 []
  (throw (ex-info "TODO: implement throw" {})))

(defop sayval 0xB9 []
  (say-value))

(defn- apply-index [obj idx]
  {:pre [(vm-primitive? obj) (number? idx)]}
  (mdo
   sq <- (as-list obj)
   (if sq
     (stack-push (nth sq (dec idx)))
     (throw (ex-info "TODO: Op overload not implemented" {})))))

(defop index 0xBA []
  (mdo
   idx <- stack-pop
   obj <- stack-pop
   (apply-index obj (value idx))))

(defop idxlcl1int8 0xBB [:ubyte local_number :ubyte index_val]
  (>>= (getlcl local_number) #(apply-index % index_val)))

(defop idxint8 0xBC [:ubyte index_val]
  (>>= stack-pop #(apply-index % index_val)))

(defop new1 0xC0 [:ubyte arg_count :ubyte metaclass_id]
  (mdo
   proto <- (get-val #(mc/prototype % metaclass_id))
   obj <- (mc/load-from-stack proto arg_count)
   (>>= (obj-intern (assoc obj :metaclass metaclass_id)) vm-return)))

(defop new2 0xC1 [:uint2 arg_count :uint2 metaclass_id]
  (throw (ex-info "TODO: implement new2" {})))

(defop trnew1 0xC2 [:ubyte arg_count :ubyte metaclass_id]
  (throw (ex-info "TODO: implement trnew1" {})))

(defop trnew2 0xC3 [:uint2 arg_count :uint2 metaclass_id]
  (throw (ex-info "TODO: implement trnew2" {})))

(defn- setlcl [i v]
  (>>= (reg-get :fp) #(stack-set (+ % i) v)))

(defn- updlcl [i f]
  (mdo
   fp <- (reg-get :fp)
   v <- (stack-get (+ fp i))
   (stack-set (+ fp i) (f v))))

(defn- updlcl-m [i mv]
  (mdo
   fp <- (reg-get :fp)
   v <- (stack-get (+ fp i))
   v+ <- (mv v)
   (stack-set (+ fp i) v+)))

(defop inclcl 0xD0 [:uint2 local_number]
  (updlcl local_number vm-inc))

(defop declcl 0xD1 [:uint2 local_number]
  (updlcl local_number vm-dec))

(defop addilcl1 0xD2 [:ubyte local_number :sbyte val]
  (updlcl-m local_number (partial compute-sum val)))

(defop addilcl4 0xD3 [:uint2 local_number :int4 val]
  (updlcl-m local_number (partial compute-sum val)))

(defop addtolcl 0xD4 [:uint2 local_number]
  (mdo
   v <- stack-pop
   (updlcl-m local_number #(compute-sum % v))))

(defop subfromlcl 0xD5 [:uint2 local_number]
  (mdo
   v <- stack-pop
   (updlcl local_number #(vm-- % v))))

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
  (>>= stack-pop #(setlcl local_number %)))

(defop setlcl2 0xE1 [:uint2 local_number]
  (>>= stack-pop #(setlcl local_number %)))

(defop setarg1 0xE2 [:ubyte arg_number]
  (mdo
   fp <- (reg-get :fp)
   v <- stack-pop
   (stack-set (- fp (+ (dec FRAME-SIZE) arg_number)) v)))

(defop setarg2 0xE3 [:uint2 arg_number]
  (mdo
   fp <- (reg-get :fp)
   v <- stack-pop
   (stack-set (- fp (+ (dec FRAME-SIZE) arg_number)) v)))

(defop setind 0xE4 []
  (throw (ex-info "TODO: implement setind" {})))

(defn- set-property
  "Updates object store to contain modified version of object
with property set as specified. oid, pid numbers. val primitive."
  [oid pid val]
  (mdo
   object <- (obj-retrieve oid)
   new-object <- (mc/set-property object pid val)
   (obj-store oid new-object)))

(defop setprop 0xE5 [:uint2 prop_id]
  (mdo
   obj-id <- stack-pop
   new-val <- stack-pop
   (set-property (value (vm-obj-check obj-id "OBJ_VAL_REQUIRED")) prop_id new-val)))

(defop ptrsetprop 0xE6 []
  (mdo
   prop-id <- stack-pop
   obj <- stack-pop
   new-val <- stack-pop
   (set-property (value obj) (value prop-id) new-val)))

(defop setpropself 0xE7 [:uint2 prop_id]
  (mdo
   new-val <- stack-pop
   self <- (get-stack-self)
   (set-property (value self) prop_id new-val)))

(defop objsetprop 0xE8 [:uint4 obj :uint2 prop_id]
  (>>= stack-pop #(set-property obj prop_id %)))

(defop setdblcl 0xE9 [:uint2 local_number]
  (throw (ex-info "TODO: implement setdblcl" {})))

(defop setdbarg 0xEA [:uint2 param_number]
  (throw (ex-info "TODO: implement setdbarg" {})))

(defop setself 0xEB []
  (throw (ex-info "TODO: implement setself" {})))

(defop loadctx 0xEC []
  (throw (ex-info "TODO: implement loadctx" {})))

(defop storectx 0xED []
  (throw (ex-info "TODO: implement storectx" {})))

(defop setlcl1r0 0xEE [:ubyte local_number]
  (>>= (reg-get :r0) (partial setlcl local_number)))

(defop setindlcl1i8 0xEF [:ubyte local_number :ubyte index_val]
  (throw (ex-info "TODO: implement setindlcli8" {})))

(defop bp 0xF1 []
  (throw (ex-info "TODO: implement bp" {})))

(defop nop 0xF2 [])

;;;
;;; Overall control.
;;;
(defn enter
  "Set up vm at initial entry point."
  [host]
  (info "Entering VM")
  (mdo
   entp <- (get-val :entry-point-offset)
   (op-pushlst host 0)
   ip <- (get-val :ip)
   (set-pc ip)
   (op-call host 1 entp)
   (commit-pc)))

(defn step
  "Execute the op code referenced by the ip register.
   pre-action takes op args ip."
  [host pre-action]
  (mdo
   ip <- (reg-get :ip)
   let _ = (tracef "IP: 0x%08x" ip)
   (if (or (nil? ip) (zero? ip))
     (reg-get :r0)
     (mdo
      ;; parse op and set pc
      [op args len] <- parse-op-at-ip

      let p = (+ ip len)
      (set-pc p)

      ;; pre-action
      (pre-action op args ip p)

      ;; call and complete
      ret <- (apply (:run-fn op) (cons host (vals args)))

      (commit-pc)
      
      (return nil)))))

(defn execute
  "Repeatedly execute a monadic step until it returns a value.
error-fn takes state and exception."
  [stepper s error-fn]
  (loop [s s]
    (let [[r s+] (try (run-state stepper s) (catch Exception e (error-fn s e)))]
      (if r
        [r s+]
        (recur s+)))))
