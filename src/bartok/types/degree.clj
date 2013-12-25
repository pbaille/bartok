(in-ns 'bartok.types )

(load "types/degree_class")

(def degrees
  (concat      
    [(with-type 'Degree {:name :R :val 0 :degree-class (degree-class :root)})]
    (for [{cn :name cv :val ddv :degree-val ct :alt-type :as dc} degree-classes
          alt (cond (= ct :t1) degree-alterations-1
                    (= ct :t2) degree-alterations-2)]
      (let [n (if (= cn :root) :R (keyword-cat (:name alt) (-> dc :val inc str)))
            v (mod12 (+ ddv (:val alt) 12))] 
        (with-type 'Degree {:name n :val v :degree-class dc})))))

(def degree-class->degree
  (reduce #(into %1 {(-> %2 :degree-class :name) %2}) {} 
          (filter #(#{:R :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) degrees)))

(def degree-default-names 
  #{:R :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})

(def name->degree (reduce #(into %1 {(:name %2) %2}) {} degrees))

(def val->degree  
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(degree-default-names (:name %)) degrees)))

;*********** construct *****************

(defmulti degree b-types)

(defmethod degree :degree [n] (name->degree n))
(defmethod degree :degree-class [g] (degree-class->degree g))
(defmethod degree :number [v] (val->degree (mod12 v)))
(defmethod degree :interval [n] (name->degree (first (dash-split n))))

;************* casts *******************

(defmethod degree 'Mode [m] (-> m :mode-class :degree))
(defmethod degree 'ModeClass [m] (:degree m))
(defmethod degree 'Interval [m] (:class m))

; *********** functions ******************

; (defn generic-val [this] 
;   (get-in this [:generic :val]))
