(ns t3chnique.monad
  (:use [clojure.algo.monads :only [defmonad with-monad state-t cont-m m-result m-bind domonad run-cont m-seq]]))

(def vm-m (state-t cont-m))

(with-monad vm-m

  (defn >> [mva mvb]
    (m-bind mva (fn [_] mvb)))

  (defn >>- [mvs]
    (reduce >> mvs))
  
  ;; primitives for working in the vm monad
  
  (defn fetch-val [key]
    (fn [s]
      (fn [k]
        (k (get key s)))))

  (defn set-val [key val]
    (fn [s]
      (fn [k]
        (k (assoc s key val)))))

  (defn copy-val [k1 k2]
    (fn [s]
      (fn [k]
        (k (assoc s k1 (get s k2))))))

  (defn unset-val [key]
    (fn [s]
      (fn [k]
        (k (dissoc s key)))))

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
                (update-in [:stack] conj val)
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
      (fn [k] e)))

  (defn vprotect [f]
    
    )

  (defn vsuspend []
    (fn [s]
      (fn [k]
        (fn [x]
          (k [x s])))))

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

  (def state
    (atom {:stack []
           :sp 0
           :ops [(vnormal) (vinput) (vnormal) (vthrows) (vnormal)]}))
  
  (defn vstep [s]
    (run-cont
     ((domonad
        [op  (code-read)
         ret op]
        ret) s))))