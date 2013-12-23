(in-ns 'bartok.types )

(load "types/degree_class")

; (def generic->interval-classes
;   {:unison  {0 :R}
;    :second  {1 :m2 2 :M2 3 :#2}
;    :third   {2 :o3 3 :m3 4 :M3 5 :#3}
;    :fourth  {4 :b4 5 :P4 6 :+4}
;    :fifth   {6 :b5 7 :P5 8 :+5}
;    :sixt    {8 :m6 9 :M6 10 :#6}
;    :seventh {9 :o7 10 :m7 11 :M7}})

; (def generic->val 
;   {:unison 0 :second 1 :third 2 :fourth 3 :fifth 4 :sixt 5 :seventh 6})

; (def interval-classes 
;   (reduce conj #{}      
;     (for [[gicn gicv] generic->interval-classes 
;           [icv icn] gicv]      
;       (with-type 
;         'IntervalClass
;         {:name icn
;          :val icv
;          :generic (generic-interval-class gicn)}))))

(def degrees
  (concat      
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

; *********** functions ******************

; (defn generic-val [this] 
;   (get-in this [:generic :val]))
