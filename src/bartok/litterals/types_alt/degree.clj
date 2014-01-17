(in-ns 'bartok.litterals.alt )

(load "types_alt/degree_class")

(def degrees
    (for [{cn :name cv :val ddv :degree-val ct :alt-type :as dc} degree-classes
          alt (cond (= ct :t1) degree-alterations-1
                    (= ct :t2) degree-alterations-2)]
      (let [n (keyword-cat (:name alt) (-> dc :val inc str))
            v (mod12 (+ ddv (:val alt) 12))] 
        (with-type 'Degree {:name n :val v :degree-class dc}))))

(def degree-class->degree
  (reduce #(into %1 {(-> %2 :degree-class :name) %2}) {} 
          (filter #(#{:P1 :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) degrees)))

(def degree-default-names 
  #{:P1 :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})

(def name->degree (reduce #(into %1 {(:name %2) %2}) {} degrees))

(def val->degree  
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(degree-default-names (:name %)) degrees)))

;*********** construct *****************

(b-construct degree
  [:degree n] (name->degree n)
  [:number v] (val->degree (mod12 v))
  ['DegreeClass dc] (degree-class->degree (:name dc))
  ['Mode m] (-> m :mode-class :degree)
  ['ModeClass m] (:degree m)
  ['Interval m] (:class m))

; *********** functions ******************

(defmethod relative 'Degree [d] 
  (degree (- 12 (:val d))))
