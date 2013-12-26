(ns bartok.harmony.modal-move
  (:use [utils.utils])
  (:use [bartok.litterals.identity])
  (:use [bartok.types]))

(def modal-moves 
  (let [mms {:SD    :P4 
             :SD-   :m6 
             :SD+   :M2 
             :SDalt :M7 
             :T     :P1 
             :T-    :m3 
             :T+    :M6 
             :Talt  :b5}]
    (conj mms (clojure.set/map-invert mms))))

(defn build-modal-move [n degree]
  (with-type 'ModalMove
    {:name n :degree degree}))

(defmulti modal-move b-types)

(defmethod modal-move :modal-move [n] 
  (build-modal-move n (degree (modal-moves n))))

(defmethod modal-move :degree [d] 
  (build-modal-move (d modal-moves) (degree d)))

(defmethod modal-move 'Degree [d] 
  (build-modal-move (-> d :name modal-moves) d))

(defmethod modal-move :interval [i] 
  (build-modal-move ((-> i degree :name) modal-moves) (degree i)))

(defmethod modal-move 'Interval [i] 
  (build-modal-move ((-> i degree :name) modal-moves) (degree i)))


