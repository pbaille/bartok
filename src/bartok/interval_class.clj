(ns bartok.interval-class
  (:use [bartok.litterals.identity])
  (:use [utils.utils]))

(def generic-interval-class
  {:root    {0 :R}
   :second  {1 :m2 2 :M2 3 :#2}
   :third   {2 :o3 3 :m3 4 :M3 5 :#3}
   :fourth  {4 :b4 5 :P4 6 :+4}
   :fifth   {6 :b5 7 :P5 8 :+5}
   :sixt    {8 :m6 9 :M6 10 :+6}
   :seventh {9 :o7 10 :m7 11 :M7}})

(def generic->val 
  {:root 0 :second 1 :third 2 :fourth 3 :fifth 4 :sixt 5 :seventh 6})

(def interval-classes 
  (reduce conj #{}      
    (for [[gicn gicv] generic-interval-class 
          [icv icn] gicv]      
      {:name icn
       :val icv
       :generic gicn})))

(def generic->default-interval-class
  (reduce #(into %1 {(:generic %2) %2}) {} 
          (filter #(#{:R :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) interval-classes)))

(def interval-class-default-names 
  #{:R :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})

(def name->interval-class (reduce #(into %1 {(:name %2) %2}) {} interval-classes))

(def val->interval-class  
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(interval-class-default-names (:name %)) interval-classes)))

;********************************************

(defrecord IntervalClass [name val generic])

;*********** construct *****************

(defn map->IntervalClass [m] (->IntervalClass (:name m) (:val m) (:generic m)))

(defmulti interval-class 
  (fn [arg]
    (cond
      (interval-class-name? arg) :name
      (generic-interval-class-name? arg) :generic
      (number? arg) :val)))

(defmethod interval-class :name [n] (map->IntervalClass (name->interval-class n)))
(defmethod interval-class :generic [g] (map->IntervalClass (generic->default-interval-class g)))
(defmethod interval-class :val [v] (map->IntervalClass (val->interval-class (mod12 v))))

;*********** functions ******************

(defn generic-val [this] 
  (generic->val (:generic this)))
