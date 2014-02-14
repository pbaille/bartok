(ns bartok.harmony.h-function
  (:use utils.utils)
  (:use bartok.primitives))

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

(b-construct h-function
  [:h-function n] 
    (build-h-function n (c-interval-class (h-functions n)))
  [:degree d] 
    (build-h-function (d h-functions) (c-interval-class d))
  ['Degree d] 
    (build-h-function (-> d :name h-functions) d)
  [:interval i] 
    (build-h-function ((-> i c-interval-class :name) h-functions) (c-interval-class i))
  ['Interval i] 
    (build-h-function ((-> i c-interval-class :name) h-functions) (c-interval-class i)))



