(in-ns 'bartok.litterals.all)

(load "types/direction")

(def generic-interval-classes 
  (map #(with-type 
          'GenericIntervalClass
          (hash-map :name %1 :val %2)) 
       '(:1st :2nd :3rd :4th :5th :6th :7th) 
       (range)))

(def name->generic-interval-class (reduce #(into %1 {(:name %2) %2}) {} generic-interval-classes))
(def val->generic-interval-class  (reduce #(into %1 {(:val %2) %2}) {} generic-interval-classes))

(defn dir-oct-expand [x] 
  (if-let [[[_ dir oct]] (re-seq #"([ud])([0-9])*" (name x))]
    [(direction (keyword dir))
     (if oct (parse-int oct) 0)]))

;************ generic interval class ******************

(b-construct generic-interval-class
  [:generic-interval-class n] 
    (name->generic-interval-class n)
  [:generic-interval gi] 
    (name->generic-interval-class (-> gi dash-split first keyword))
  [:number v] 
    (val->generic-interval-class v))

;*************** generic interval ***************

(b-construct generic-interval 
  [:generic-interval n] 
    (let [[gin diroct] (dash-split n)
           class (generic-interval-class (keyword gin))
           [dir oct] (dir-oct-expand diroct)
           val (* (:val dir) (+ (:val class) (* 7 oct)))]
      (with-type 'GenericInterval 
                 {:name n :val val :class class :direction dir :octave-offset oct}))
  
  [:generic-interval-class n]
    (generic-interval (keyword-cat n "-u"))
  
  ['GenericIntervalClass gic 'Direction d]
    (generic-interval (keyword-cat (:name gic) "-" (:name d)))
    
  [:number v]
    (let [[oct m] (div-mod (abs v) 7)
          class (generic-interval-class m)
          dir (direction (if (>= v 0) :u :d))
          n (keyword-cat (:name class) "-" (:name dir) (if (= 0 oct) "" (str oct)))]
      (with-type 'GenericInterval 
                 {:name n :val v :class class :direction dir :octave-offset oct})))
