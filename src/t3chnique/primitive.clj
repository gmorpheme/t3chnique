(ns t3chnique.primitive)

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
