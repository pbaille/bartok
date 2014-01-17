(ns bartok.harmony.h-function
  (:use utils.utils)
  (:use bartok.litterals.all))

(def h-functions 
  (let [mms {:SD    :P4 
             :SD-   :m6 
             :SD+   :M2 
             :SDalt :M7 
             :T     :P1 
             :T-    :m3 
             :T+    :M6 
             :Talt  :b5}]
    (conj mms (clojure.set/map-invert mms))))

(defn build-h-function [n degree]
  (with-type 'ModalMove
    {:name n :degree degree}))

(defmulti h-function b-types)

(defmethod h-function :h-function [n] 
  (build-h-function n (degree (h-functions n))))

(defmethod h-function :degree [d] 
  (build-h-function (d h-functions) (degree d)))

(defmethod h-function 'Degree [d] 
  (build-h-function (-> d :name h-functions) d))

(defmethod h-function :interval [i] 
  (build-h-function ((-> i degree :name) h-functions) (degree i)))

(defmethod h-function 'Interval [i] 
  (build-h-function ((-> i degree :name) h-functions) (degree i)))


