(in-ns 'bartok.types)

(def natural-pitch-classes
  (reduce #(conj %1 {:name (first %2) :val (second %2)})
          #{} {:A 9 :B 11 :C 0 :D 2 :E 4 :F 5 :G 7}))

(def natural-pitch-classes-names [:A :B :C :D :E :F :G])

(def name->natural-pitch-class (reduce #(into %1 {(:name %2) %2}) {} natural-pitch-classes))
(def val->natural-pitch-class  (reduce #(into %1 {(:val %2) %2}) {} natural-pitch-classes))

;*******************************************

;(declare natural-pitch-class)

; (defrecord NaturalPitchClass [name val]
;   Transpose
;   (transpose [this n] 
;     (let [current-index (index-of name natural-pitch-classes-names)
;           new-name (nth natural-pitch-classes-names (mod (+ current-index n) 7))]
;       (natural-pitch-class new-name))))

;************ construct ******************

(defmulti natural-pitch-class b-type )

(defmethod natural-pitch-class :natural-pitch-class [n] (name->natural-pitch-class n))
(defmethod natural-pitch-class :number [v] (val->natural-pitch-class v))

;**************** methods ****************

; (defmethod transpose ['NaturalPitchClass ] [this n])