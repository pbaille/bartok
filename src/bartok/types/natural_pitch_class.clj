(in-ns 'bartok.types)

(def natural-pitch-classes
  (reduce #(conj %1 (with-type 
                      'NaturalPitchClass 
                      {:name (first %2) :val (second %2)}))
          #{} {:A 9 :B 11 :C 0 :D 2 :E 4 :F 5 :G 7}))

(def natural-names [:A :B :C :D :E :F :G])

(def name->natural-pitch-class (reduce #(into %1 {(:name %2) %2}) {} natural-pitch-classes))
(def val->natural-pitch-class  (reduce #(into %1 {(:val %2) %2}) {} natural-pitch-classes))

;************ construct ******************

(defmulti natural-pitch-class b-types)

(defmethod natural-pitch-class :natural-pitch-class [n] (name->natural-pitch-class n))
(defmethod natural-pitch-class :number [v] (val->natural-pitch-class v))

;**************** methods ****************

(defmethod transpose 'NaturalPitchClass [this generic-interval]
  (let [c (index-of (:name this) natural-names)
        new-name (get (-> natural-names (rotate (:val generic-interval)) vec ) c)]
    (natural-pitch-class new-name)))