(ns t3chnique.monad
  (:use [clojure.algo.monads]))

(def vm-m state-m)

(defmacro do-vm
  "Sugar: 'do'-syntax for running monadic values in the vm monad."
  [bindings val]
  `(domonad vm-m ~bindings ~val))

(defmacro in-vm
  "Sugar: run mv in the vm monad."
  [mv]
  `(with-monad vm-m ~mv))

(defn m-apply
  "Return result of applying f to state, do not evolve state."
  [f & args]
  {:pre [f]}
  (fn [s] {:pre [s]} [(apply f s args) s]))

(defn abort
  "For now, throw - incorporate into monad later."
  [msg]
  (throw (RuntimeException. ^String msg)))

(comment
;;; experimental implementation with cont-m  
  (def vm-m (state-t cont-m))

  ;; experimenting with a state & continuation monad
  
  ;; continuation required to provide yield capability and potentially
  ;; early exist / revert

  (with-monad vm-m

    (defn >> [mva mvb]
      (m-bind mva (fn [_] mvb)))

    (defn >>- [mvs]
      (reduce >> mvs))
    
    ;; primitives for working in the vm monad

    (defn call-cc [f]
      (fn [s]
        (fn [k]
          (let [cc (fn [x]
                     (fn [_s] (fn [_k] (k [x s]))))
                rc (f cc)]
            ((rc s) k))))) ; ?
    
    (defn fetch-val [key]
      (fn [s]
        (fn [k]
          (k [(get key s) s]))))

    (defn set-val [key val]
      (fn [s]
        (fn [k]
          (k [nil (assoc s key val)]))))

    (defn copy-val [k1 k2]
      (fn [s]
        (fn [k]
          (k [nil (assoc s k1 (get s k2))]))))

    (defn unset-val [key]
      (fn [s]
        (fn [k]
          (k [nil (dissoc s key)]))))

    ;; machine primitives
    (def fresh-pc (copy-val :ip :pc))
    (def set-pc (partial set-val :pc))
    (def commit-pc )
    
    (defn code-read []
      (fn [s]
        (fn [k]
          (k [(first (:ops s))
              (-> s
                  (update-in [:ops] rest))]))))
    
    (defn stack-push [x]
      (fn [s]
        (fn [k]
          (k [nil
              (-> s
                  (update-in [:stack] conj x)
                  (update-in [:sp] inc))]))))

    (defn stack-pop []
      (fn [s]
        (fn [k]
          (k [(last (:stack s))
              (-> s
                  (update-in [:stack] pop)
                  (update-in [:sp] dec))]))))

    (defn vabort [e]
      (fn [s]
                                        ; don't all continuation just return e and state as is
        (fn [k] [e s])))

    (defn vsuspend []
      (fn [s]
        (fn [k]
          [(fn [x] (k [x s])) s])))

    ;; examples
    
    (defn vinput []
      (domonad 
          [_ (stack-push 5)
           x (vsuspend)
           _ (stack-pop)
           _ (stack-push x)]
        nil))

    (defn vthrows []
      (domonad 
          [_ (stack-push 999)
           _ (vabort "error")
           _ (stack-push 7)
           x (stack-pop)
           y (stack-pop)]
        x))

    (defn vnormal []
      (domonad 
          [_ (stack-push 5)
           _ (stack-push 7)
           x (stack-pop)]
        x))

    (defn state []
      {:stack []
       :sp 0
       :ops [(vnormal) (vinput) (vnormal) (vthrows) (vnormal)]})
    
    (defn vstep [s]
      (run-cont
       ((domonad
            [op  (code-read)
             ret op]
          ret) s)))))

(comment

  ;; to try
  (run-cont ((vnormal) (state))) ; => [7 ...]

  (def five (domonad vm-m
              [_ (call-cc
                  (fn [exit]
                    (exit 5)))] 12)) ; ?

  (run-cont ((five) (state)))

  )