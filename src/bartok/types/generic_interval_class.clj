(in-ns 'bartok.types)

(def generic-interval-classes 
  (map #(hash-map :name %1 :val %2) 
       '(:unison :second :third :fourth :fifth :sixt :seventh) 
       (range)))

(def name->generic-interval-class (reduce #(into %1 {(:name %2) %2}) {} generic-interval-classes))
(def val->generic-interval-class  (reduce #(into %1 {(:val %2) %2}) {} generic-interval-classes))

;*******************************************

(declare generic-interval-class)

(defrecord GenericIntervalClass [name val])

;************ construct ******************

(defn map->GenericIntervalClass [m] (->GenericIntervalClass (:name m) (:val m)))

(defmulti generic-interval-class
  (fn [arg]
    (cond
      (generic-interval-class-name? arg) :name
      (between arg [0 6]) :val)))

(defmethod generic-interval-class :name [n] (map->GenericIntervalClass (name->generic-interval-class n)))
(defmethod generic-interval-class :val [v] (map->GenericIntervalClass (val->generic-interval-class v)))

;*************** generic interval ***************

(defn split-generic-interval-name [x]
  (if (named? x) 
    (let [[_ nam oct] (re-matches #"([a-z]*)([0-9])" (name x))
          nam (keyword nam)
          oct (parse-int oct)]
      (if (and (generic-interval-class-name? nam) (between oct 0 9)) [nam oct] nil))))


(defmulti generic-interval
  (fn [arg]
    (cond
      (generic-interval-class-name? arg) :class-name
      (split-generic-interval-name arg) :name
      (number? arg) :val)))

(defmethod generic-interval :name [n] 
  (let [[gin oct] (split-generic-interval-name n)
         class (generic-interval-class gin)
         val (+ (:val class) (* 7 oct))]
    (with-type 'GenericInterval 
               {:name n :val val :class class :octave oct})))

(defmethod generic-interval :class-name [n]
  (generic-interval (keyword-cat n "0")))
