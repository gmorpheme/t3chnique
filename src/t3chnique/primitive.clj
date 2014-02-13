(ns t3chnique.primitive)

;; primitives
(defrecord TypedValue [type value])
(defrecord Primitive [name typeid doc encoding])

(def primitives (atom {}))

(defmacro defprimitive [name typeid doc encoding]
  `(let [p# (Primitive. (keyword '~name) ~typeid ~doc ~encoding)]
     (swap! primitives assoc ~typeid p#)
     (defn ~name
       ([val#] (TypedValue. ~typeid val#))
       ([] (TypedValue. ~typeid nil)))
     (defn ~(symbol (str name "?")) [entry#] (= (:type entry#) ~typeid))
     (def ~(symbol (str name "-id")) ~typeid)))

(defn primitive [typeid] (get @primitives typeid))
(defn typed-value [typeid value] (TypedValue. typeid value))

(defprimitive vm-nil 1 "nil (boolean \"false\" or null pointer)" nil)
(defprimitive vm-true 2 "boolean \"true\"" nil)
(defprimitive vm-stack 3 "Reserved for implementation use for storing native machine pointers to stack frames (see note below)" nil)
(defprimitive vm-codeptr 4 "Reserved for implementation use for storing native machine pointers to code (see note below)" nil)
(defprimitive vm-obj 5 "object reference as a 32-bit unsigned object ID number" :uint4)
(defprimitive vm-prop 6 "property ID as a 16-bit unsigned number" :uint2)
(defprimitive vm-int 7 "integer as a 32-bit signed number" :int4)
(defprimitive vm-sstring 8 "single-quoted string; 32-bit unsigned constant pool offset" :uint4)
(defprimitive vm-dstring 9 "double-quoted string; 32-bit unsigned constant pool offset" :uint4)
(defprimitive vm-list 10 "list constant; 32-bit unsigned constant pool offset" :uint4)
(defprimitive vm-codeofs 11 "code offset; 32-bit unsigned code pool offset" :uint4)
(defprimitive vm-funcptr 12 "function pointer; 32-bit unsigned code pool offset" :uint4)
(defprimitive vm-empty 13 "no value (this is useful in some cases to represent an explicitly unused data slot, such as a slot that has never been initialized)" nil)
(defprimitive vm-native-code 14 "Reserved for implementation use for storing native machine pointers to native code (see note below)" nil)
(defprimitive vm-enum 15 "enumerated constant; 32-bit integer" :uint4)
(defprimitive vm-bifptr 16 "built-in function pointer; 32-bit integer, encoding the function set dependency table index in the high-order 16 bits, and the function's index within its set in the low-order 16 bits." :uint4)
(defprimitive vm-objx 17 "Reserved for implementation use for an executable object, as a 32-bit object ID number (see note below)" :uint4)

(def vm-obj-or-nil? (some-fn vm-obj? vm-nil?))
(def vm-string? (some-fn vm-sstring? vm-dstring?))
(def vm-auto-eval? (some-fn vm-codeofs? vm-dstring?))

(defn vm-bool [v] (if v (vm-true) (vm-nil)))
(defn typeid [entry] (:type entry))
(defn value [entry] (:value entry))
(defn vm-zero? [v] (and (vm-int? v) (zero? (value v))))
(defn vm-primitive? [v] (and
                         (contains? v :type)
                         (contains? v :value)))
(defn valid? [v]
  (cond
   (vm-nil? v) false
   (vm-obj? v) (and (value v) (pos? (value v)))
   (vm-prop? v) (and (value v) (pos? (value v)))
   (vm-objx? v) (and (value v) (pos? (value v)))
   (vm-funcptr? v) (and (value v) (pos? (value v)))
   :else true))

(defn mnemonise
  "Return a short string representation of the primitive for use in tooling
/ debugging interfaces."
  [val]
  (condp = (typeid val)
    vm-nil-id "nil"
    vm-true-id "t"
    vm-stack-id (str "st" (value val))
    vm-codeptr-id (str "#" (value val))
    vm-obj-id (str "o" (value val))
    vm-prop-id (str "p" (value val))
    vm-int-id (str (value val))
    vm-sstring-id (str "'" (value val))
    vm-dstring-id (str \" (value val))
    vm-list-id (str "[]" (value val))
    vm-codeofs-id (str "x" (value val))
    vm-funcptr-id (str "f" (value val))
    vm-empty-id "?0"
    vm-native-code-id "?x"
    vm-enum-id (str "e{}" (value val))
    vm-bifptr-id (str "b" (value val))
    vm-objx-id (str "ox" (value val))))

(defn bif-set-and-index
  "Parse the function set and function index out of a bif pointer."
  [{v :value :as p}]
  {:pre [(vm-bifptr? p)]}
  [(bit-and 0xffff0000 v) (bit-and 0xffff v)])
