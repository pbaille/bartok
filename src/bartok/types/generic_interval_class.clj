(in-ns 'bartok.types)

(def generic-interval-classes 
  (map #(hash-map :name %1 :val %2) 
       '(:unison :second :third :fourth :fifth :sixt :seventh) 
       (range)))

(def name->generic-interval-class (reduce #(into %1 {(:name %2) %2}) {} generic-interval-classes))
(def val->generic-interval-class  (reduce #(into %1 {(:val %2) %2}) {} generic-interval-classes))

;************ generic interval class ******************

(defmulti generic-interval-class b-types )

(defmethod generic-interval-class :generic-interval-class [n] 
  (with-type 'GenericIntervalClass (name->generic-interval-class n)))
(defmethod generic-interval-class :number [v] 
  (with-type 'GenericIntervalClass (val->generic-interval-class v)))

;*************** generic interval ***************

(defmulti generic-interval b-types)

(defmethod generic-interval :generic-interval [n] 
  (let [[_ gin oct] (re-matches #"([a-z]*)([0-9])" (name n))
         class (generic-interval-class gin)
         val (+ (:val class) (* 7 oct))]
    (with-type 'GenericInterval 
               {:name n :val val :class class :octave oct})))

(defmethod generic-interval :generic-interval-class [n]
  (generic-interval (keyword-cat n "0")))
