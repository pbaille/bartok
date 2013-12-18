(in-ns 'bartok.types)

(load "types/interval_class")

(def directions {:u 1 :d -1})

(def intervals 
  (reduce conj #{}      
    (for [{icn :name icv :val icg :generic :as ic} interval-classes 
           oct (range 8)
          [dirn dirv] directions]      
      {:name (keyword (str (name icn) (name dirn) (if (= 0 oct) "" oct)))
       :val (* dirv (+ icv (* 12 oct)))
       :direction {:name dirn :val dirv}
       :octave-offset oct
       :interval-class (map->IntervalClass ic)})))

(def name->interval (reduce #(into %1 {(:name %2) %2}) {} intervals))

(def val->interval 
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(interval-class-default-names (get-in % [:interval-class :name])) 
                  intervals)))

(defrecord Interval [name val direction octave-offset interval-class])

(defn map->Interval [m] (->Interval (:name m) (:val m) (:direction m) (:octave-offset m) (:interval-class m)))

(defmulti interval
  (fn [arg]
    (cond
      (interval-name? arg) :name
      (interval-class-name? arg) :interval-class
      (generic-interval-class-name? arg) :generic
      (number? arg) :val)))

(defmethod interval :name [n] (map->Interval (name->interval n)))
(defmethod interval :interval-class [ic] (map->Interval (name->interval (keyword-cat ic :u))))
(defmethod interval :generic [g] 
  (map->Interval 
    (name->interval 
      (keyword-cat (:name (generic->default-interval-class g)) 
                    :u))))
(defmethod interval :val [v] (map->Interval (val->interval v)))




