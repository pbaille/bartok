(ns bartok.m-degree
  (:use [bartok.litterals.identity])
  (:use [bartok.constants])
  (:use [utils.utils]))

(def generic-m-degrees 
  {:root    {0 :R}
   :second  {1 :m2 2 :M2 3 :#2}
   :third   {2 :o3 3 :m3 4 :M3 5 :#3}
   :fourth  {4 :b4 5 :P4 6 :+4}
   :fifth   {6 :b5 7 :P5 8 :+5}
   :sixt    {8 :m6 9 :M6 10 :+6}
   :seventh {9 :o7 10 :m7 11 :M7}})

(def m-degrees 
  (reduce conj #{}      
    (for [[amdn amdv] generic-m-degrees 
          [mdv mdn] amdv]      
      {:name mdn
       :val mdv
       :generic amdn})))

(def generic->default-m-degree 
  (reduce #(into %1 {(:generic %2) %2}) {} 
          (filter #(#{:R :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) m-degrees)))

(def name->m-degree (reduce #(into %1 {(:name %2) %2}) {} m-degrees))
(def val->m-degree  (reduce #(into %1 {(:val %2) %2}) {} m-degrees))

(defrecord MDegree [name val generic])

(defn map->MDegree [m] (->MDegree (:name m) (:val m) (:generic m)))

(defmulti m-degree 
  (fn [arg]
    (cond
      (m-degree-name? arg) :name
      (generic-m-degree-name? arg) :generic
      (between arg [-2 2]) :val)))

(defmethod m-degree :name [n] (map->MDegree (name->m-degree n)))
(defmethod m-degree :generic [g] (map->MDegree (generic->default-m-degree g)))
(defmethod m-degree :val [v] (map->MDegree (val->m-degree v)))


