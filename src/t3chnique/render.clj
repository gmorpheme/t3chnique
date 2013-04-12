(ns t3chnique.render
  (:import [t3chnique.primitive TypedValue]))

(defprotocol Render
  (render-raw [self])
  (render [self state]))


(extend-protocol Render
  TypedValue
  (render-raw [self]
    )
  (render [self state]
    (cond
     (vm-nil? self) "[nil]"
     (vm-true? self) "[true]"
     (vm-obj? self) (format "[oid:%d]" (value self))
     (vm-prop? self) (format "")
     ))
    )