(in-ns 'bartok.types)

(load "types/pitch_class")

(def pitches 
  (reduce conj #{}      
    (for [{pcn :name pcv :val} pitch-classes 
          oct (range -6 7)]
      (let [val (+ (* 12 (+ oct 5)) pcv)]
        (when (between val 0 127 )
          (with-type 
            'Pitch
            {:name (keyword (str (name pcn) oct))
             :val val
             :octave oct
             :pitch-class (pitch-class pcn)}))))))

; pitches with either no or b alteration
(def default-name-pitches 
  (let [defaults (set pitch-class-defaults-names)]
    (filter #( defaults (get-in % [:pitch-class :name])) pitches)))

(def name->pitch (reduce #(into %1 {(:name %2) %2}) {} pitches))
(def val->pitch  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitches))

; (defrecord Pitch [name val octave pitch-class]
;   Transpose
;   (transpose [this interval]
;     (let [pc (transpose pitch-class (:interval-class interval))
;           v (+ (:val this) (:val interval))
;           o (+ octave (:octave-offset interval))
;           n (keyword-cat (:name pc) (keyword (str o)))]
;       (->Pitch n v o pc))))
  
;; type check
;(defn pitch? [x] (instance? Pitch x))

; ;*********** Constructor ***********

(defmulti pitch b-type )

(defmethod pitch :number [v] (val->pitch v))
(defmethod pitch :pitch [n] (name->pitch n))

(defmethod pitch 'PitchClass [pc] 
  (pitch (keyword-cat (:name pc) "0")))

(defmethod pitch [:pitch-class :number] [p o]
  (pitch (keyword-cat p (str o))))

;**************** functions ******************

(defn distance [p1 p2]
  {:pre [(and (pitch? p1) (pitch? p2))]}
  (abs (- (:val p1) (:val p2))))


