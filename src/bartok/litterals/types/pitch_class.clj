(in-ns 'bartok.litterals.types)

(load "types/alteration")
(load "types/natural_pitch_class")

(def pitch-class)

(def pitch-classes 
  (reduce conj #{}      
    (for [{npcn :name npcv :pitch-val} natural-pitch-classes 
          {an :name av :val} pitch-alterations]
      (with-type 
        'PitchClass
        {:name (if (= av 0) (keyword npcn) (keyword-cat npcn an))
         :val (mod (+ npcv av 12) 12)
         :natural (natural-pitch-class npcn)
         :alteration (alteration av)}))))

(def pitch-class-defaults-names 
  #{:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B})

(def default-name-pitch-classes 
  (filter pitch-class-defaults-names pitch-classes))

; for performance
(def name->pitch-class (reduce #(into %1 {(:name %2) %2}) {} pitch-classes))
(def val->pitch-class  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitch-classes))

;;*********** Constructor ***********

(defmulti pitch-class b-types )

(defmethod pitch-class :number [v] (val->pitch-class v))

(defmethod pitch-class :pitch-class [n] (name->pitch-class n))

(defmethod pitch-class :natural-pitch-class [n] (name->pitch-class n))

(defmethod pitch-class 'Pitch [p] (:pitch-class p))

(defmethod pitch-class 'PitchClass [pc] pc )

(defmethod pitch-class [:natural-pitch-class :number] [n v]
  (select-first #(and (= (get-in % [:natural :name]) n)
                      (= (:val %) v))
                pitch-classes))

;*************** functions **************

(defmethod transpose ['PitchClass 'Interval] [pc i]
    (let [nat (:name (transpose (:natural pc) (-> i :generic :val)))
          v (mod12 (+ (:val pc) (:val i)))]
      (pitch-class nat v)))






