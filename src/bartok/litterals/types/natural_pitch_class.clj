(in-ns 'bartok.litterals.all)

(declare generic-interval)

(def natural-pitch-classes
  (map #(with-type 'NaturalPitchClass (zipmap [:pitch-val :val :name] [%3 %2 %1]))
       ['C 'D 'E 'F 'G 'A 'B] (range) [0 2 4 5 7 9 11]))

(def name->natural-pitch-class (reduce #(into %1 {(:name %2) %2}) {} natural-pitch-classes))
(def val->natural-pitch-class  (reduce #(into %1 {(:val %2) %2}) {} natural-pitch-classes))

;************ construct ******************

(b-construct natural-pitch-class 
  [:natural-pitch-class n] (name->natural-pitch-class n)
  [:number v] (val->natural-pitch-class (mod v 7)))

;**************** methods ****************

(b-meth transpose ['NaturalPitchClass 'GenericInterval] [this gi]
  (natural-pitch-class (+ (:val this) (:val gi))))

(b-meth transpose ['NaturalPitchClass :number] [this n]
  (natural-pitch-class (+ (:val this) n)))