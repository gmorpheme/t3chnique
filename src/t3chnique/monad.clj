(ns t3chnique.monad
  (:use [clojure.algo.monads :only [defmonad with-monad state-t cont-m m-result m-bind domonad run-cont m-seq]]))

(def vm-m (state-t cont-m))

(with-monad vm-m
  ;; state-t cont-m
  (defn vpush [x] (fn [s] (fn [k] (k [nil (conj s x)]))))
  (defn vpop [] (fn [s] (fn [k] (k [(last s) (butlast s)]))))
  (defn vabort [e] (fn [s] (fn [k] e)))
  (defn vsuspend [] (fn [s] (fn [k] (fn [x] (k [x s])))))

  (defn vinput []
    (domonad 
             [_ (vpush 5)
              x (vsuspend)
              _ (vpop)
              _ (vpush x)]
             nil))

  (defn vthrows []
    (domonad 
             [_ (vpush 999)
              _ (vabort "error")
              _ (vpush 7)
              x (vpop)
              y (vpop)]
             x))

  (defn vnormal []
    (domonad 
             [_ (vpush 5)
              _ (vpush 7)
              x (vpop)]
             x))

  (def state (atom {:stack [] :cont nil :ops [(vnormal) (vinput) (vnormal) (vthrows) (vnormal)]}))

  (defn process-state [s]
    (let [op (first (:ops s))
          s (update-in s [:ops] rest)
          r (run-cont (op (:stack s)))]
      (cond
       (seq r) (assoc s :stack (second r))
       (fn? r) (assoc s :cont r))))
  
  (defn vcall []
    (doseq [op [(vnormal) (vinput) (vnormal) (vthrows) (vnormal)]]
      (let [r (run-cont (op (:stack @session)))]
        (cond
         (seq r) (swap! s ))
        ))))