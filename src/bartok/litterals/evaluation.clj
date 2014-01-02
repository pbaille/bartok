(ns bartok.litterals.evaluation
  (:use [bartok.litterals.identity])
  (:use [bartok.litterals.patterns])
  (:use [utils.utils])
  (:use [bartok.types]))

(declare comp-b>)

(defn b> 
  ([x] (when-let [t (b-type x)] 
         (cond 
           (or (= t :number)(= t :ratio)) x
           (keyword? t) (call (name t) x) 
           (fn? x) (comp-b> x)
           (vector? x) (vec (map b> x))
           (set? x) (set (map b> x))
           :else x)))
  ([x & xs] (map b> (cons x xs))))

(defn comp-b> [f]
  #(apply f (map b> %&)))

(defn b>> [f & args]
  (apply (comp-b> f) args))

(defmacro b-fn [n args & body]
  `(do (defn ~n ~args ~@body)
       (def  ~n (comp-b> ~n))))

(defmacro b-def [n x] 
  `(def ~n (b> ~x)))

; (defmacro befmulti [n disp]
;   `(defmulti ~n (comp-b> ~disp)))

; (defmacro befmethod [n disp-val args & body]
;   `(defmethod ~n ~disp-val ~args 
;      (comp-b> ~disp)))













