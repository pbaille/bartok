(ns bartok.litterals.evaluation
  (:use utils.utils)
  (:use [bartok.litterals.types])
  (:use [bartok.litterals.patterns])
  (:use [bartok.litterals.identity]))

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

(defmacro b-multi [n disp]
  (let [sym (symbol (str "b-multi-" n))]
    `(do 
       (defmulti ~sym ~disp)
       (b-fn ~n [& args#] (a ~sym args#)))))

(defmacro b-method [n disp-val args & body]
  (let [sym (symbol (str "b-multi-" n))]
    `(defmethod ~sym ~disp-val ~args ~@body)))

; (defmacro b-construct [n]
;   (let [sym (symbol (str "b-multi-" n))]
;     `(do 
;        (defmulti ~sym b-types)
;        (defn ~n [& args#] 
;          (if (= (keyword '~n) (b-types (first args#)))
;            (~sym (first args#))
;            (a ~sym (b> args#)))))))












