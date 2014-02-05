(ns experiments
  (:require [t3chnique.monad :as m]
            [t3chnique.vm :as vm]
            [t3chnique.intrinsics :as bif]
            [t3chnique.primitive :as p]
            [clojure.algo.monads :refer :all]
            [clojure.core.async :refer [go go-loop <! >!]]
            [clojure.core.async.impl.protocols :refer [Channel]]))

(defn channel? [x]
  (satisfies? x Channel))

;;;;;;;;;;; state and async
(declare resumable-bif)

;; proposed new expansion of defop
;;
;; async ops return monadic values which might not return [r s] but
;; instead a channel for 
(do
  (defn ^{:opcode 0xbd :async true} op-bif-like [host]
    (fn [s]
      (let [c ((resumable-bif host 0) s)]
        (go
          (let [[ret s] (<! c)]
            (m/run-vm (vm/return (p/vm-nil)) s)))))))

;; monadic value is now [r s] | Chan([r s])
;; is it still a monad?

;; if host satisfies IResumable, bifs return channel
(defn resumable-bif [host argc]
  {:pre [(satisfies? bif/IAsyncHost host)]}
  (let [out (bif/output-channel host)
        in (bif/input-channel host)]
    (fn [s]
      (go
        (>! out "question?")
        (let [answer (<! in)]
          (println "received input: " answer)
          (m/run-vm (vm/return answer) s))))))

(defn step [host]
  (m/do-vm
   [ip (:fetch-val :ip)
    [buffer index] (m/m-apply vm/offset ip)
    :let [[[op args] [_ new-index]] ((vm/parse-op) [buffer index])
          f (:run-fn op)]
    _ (update-val :ip (partial + (- new-index index)))
    ret (apply f host (vals args))]
   ret)) ; may be channel

(defn main-loop-async [host state]
  {:pre [(satisfies? bif/IAsyncHost host)]}
  (go-loop []
    (let [ret ((step host) state)
          ret (if (channel? ret) (<! ret) ret)
          [r s] ret]
      (if-let [e (or r (:exc state))]
        (println e)
        (recur)))))

(defrecord TestHost [in out]
    bif/IAsyncHost
    (input-channel [_] in)
    (output-channel [_] out))


;;;;;;;;;;;;;;;; continuations

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
