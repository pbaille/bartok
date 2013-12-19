(in-ns 'bartok.types )

(load "types/generic_interval_class")

(def generic->interval-classes
  {:unison  {0 :R}
   :second  {1 :m2 2 :M2 3 :#2}
   :third   {2 :o3 3 :m3 4 :M3 5 :#3}
   :fourth  {4 :b4 5 :P4 6 :+4}
   :fifth   {6 :b5 7 :P5 8 :+5}
   :sixt    {8 :m6 9 :M6 10 :#6}
   :seventh {9 :o7 10 :m7 11 :M7}})

(def generic->val 
  {:unison 0 :second 1 :third 2 :fourth 3 :fifth 4 :sixt 5 :seventh 6})

(def interval-classes 
  (reduce conj #{}      
    (for [[gicn gicv] generic->interval-classes 
          [icv icn] gicv]      
      (with-type 
        'IntervalClass
        {:name icn
         :val icv
         :generic (generic-interval-class gicn)}))))

(def generic->default-interval-class
  (reduce #(into %1 {(:generic %2) %2}) {} 
          (filter #(#{:R :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) interval-classes)))

(def interval-class-default-names 
  #{:R :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})

(def name->interval-class (reduce #(into %1 {(:name %2) %2}) {} interval-classes))

(def val->interval-class  
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(interval-class-default-names (:name %)) interval-classes)))

;*********** construct *****************

(defmulti interval-class b-type )

(defmethod interval-class :interval-class [n] (name->interval-class n))
(defmethod interval-class :generic-interval-class [g] (generic->default-interval-class g))
(defmethod interval-class :number [v] (val->interval-class (mod12 v)))

;*********** functions ******************

(defn generic-val [this] 
  (get-in this [:generic :val]))
