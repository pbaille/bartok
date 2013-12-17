(ns bartok.pitch
  (:use [bartok.litterals.identity])
  (:use [bartok.protocols])
  (:use [bartok.pitch-class])
  (:use [utils.utils]))

(def pitches 
  (reduce conj #{}      
    (for [{pcn :name pcv :val} pitch-classes 
          oct (range -6 7)]
      (let [val (+ (* 12 (+ oct 5)) pcv)]
        (when (between val 0 127 )
          {:name (keyword (str (name pcn) oct))
           :val val
           :octave oct
           :pitch-class (pitch-class pcn)})))))

; pitches with either no or b alteration
(def default-name-pitches 
  (let [defaults (set pitch-class-defaults-names)]
    (filter #( defaults (get-in % [:pitch-class :name])) pitches)))

(def name->pitch (reduce #(into %1 {(:name %2) %2}) {} pitches))
(def val->pitch  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitches))

(defrecord Pitch [name val octave pitch-class]
  Transpose
  (transpose [this interval]
    (let [pc (transpose pitch-class (:interval-class interval))
          v (+ (:val this) (:val interval))
          o (+ octave (:octave-offset interval))
          n (keyword-cat (:name pc) (keyword (str o)))]
      (->Pitch n v o pc))))
  
;; type check
(defn pitch? [x] (instance? Pitch x))

; ;*********** Constructor ***********

(defn map->Pitch [m]
  (let [{:keys [name val octave pitch-class]} m]
    (->Pitch name val octave pitch-class)))

(defmulti pitch
  (fn [& args]
    (let [[a b] args]
      (cond
        (number? a)      :val
        (pitch-name? a)  :name
        (pitch-class? a) :pitch-class
        (map? a)         :map
        (and (pitch-class-name? a)
             (number? b)) 
          [:pitch-class :octave]))))

(defmethod pitch :val [v] (map->Pitch (val->pitch v)))

(defmethod pitch :name [n] (map->Pitch (name->pitch n)))

(defmethod pitch :pitch-class [pc] 
  (pitch (keyword-cat (:name pc) "0")))

(defmethod pitch :map [m]
  (map->Pitch (first-where m pitches)))

(defmethod pitch [:pitch-class :octave] [p o]
  (map->Pitch (first-where {:pitch-class p :octave o} pitches)))


