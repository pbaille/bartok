(in-ns 'bartok.types)

(load "types/interval_class")

(def directions {:u 1 :d -1})

(def intervals 
  (reduce conj #{}      
    (for [{icn :name icv :val icg :generic :as ic} interval-classes 
           oct (range 8)
          [dirn dirv] directions]      
      (with-type
        'Interval
        {:name (keyword (str (name icn) (name dirn) (if (= 0 oct) "" oct)))
         :val (* dirv (+ icv (* 12 oct)))
         :direction {:name dirn :val dirv}
         :octave-offset oct
         :interval-class ic}))))

(def name->interval (reduce #(into %1 {(:name %2) %2}) {} intervals))

(def val->interval 
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(interval-class-default-names (get-in % [:interval-class :name])) 
                  intervals)))

(defmulti interval b-types )

(defmethod interval :interval [n] (name->interval n))
(defmethod interval :interval-class [ic] (name->interval (keyword-cat ic :u)))
(defmethod interval :generic-interval-class [g] 
  (name->interval (keyword-cat (:name (generic->default-interval-class g)) :u)))

(defmethod interval :number [v] (val->interval v))




