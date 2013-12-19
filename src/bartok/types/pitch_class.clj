(in-ns 'bartok.types)

(load "types/alteration")
(load "types/natural_pitch_class")
(load "types/interval_class")

(declare pitch? )


(def pitch-classes 
  (reduce conj #{}      
    (for [{npcn :name npcv :val} natural-pitch-classes 
          {an :name av :val} alterations]
      (with-type 
        'PitchClass
        {:name (if (= av 0) npcn (keyword-cat npcn an))
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

;*********************************************

; (declare pitch-class)

; (defrecord PitchClass [name val natural alteration]
;   Transpose
;   (transpose [this interval]
;     (let [nat (:name (transpose natural (get-in interval [:generic :val])))
;           v (mod12 (+ (:val this) (:val interval)))]
;       (pitch-class nat v))))

;; type check
;(defn pitch-class? [x] (instance? PitchClass x))

;;*********** Constructor ***********

; (defn map->PitchClass [m]
;   (let [{:keys [name val natural alteration]} m]
;     (->PitchClass name val natural alteration)))

(defmulti pitch-class b-type )

(defmethod pitch-class :number [v] (val->pitch-class v))

(defmethod pitch-class :pitch-class [n] (name->pitch-class n))

(defmethod pitch-class :natural-pitch-class [n] (name->pitch-class n))

(defmethod pitch-class 'Pitch [p] (:pitch-class p))

(defmethod pitch-class 'PitchClass [pc] pc )

(defmethod pitch-class [:natural-pitch-class :number] [n v]
  (select-first #(and (= (get-in % [:natural :name]) n)
                      (= (:val %) v))
                pitch-classes))
