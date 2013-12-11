(ns bartok.pitch
  (:use [bartok.constants])
  (:use [bartok.litterals.identity])
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

(defrecord Pitch [name val octave pitch-class])
  

; ;*********** Constructor ***********

(defn map->Pitch [m]
  (let [{:keys [name val octave pitch-class]} m]
    (->Pitch name val octave pitch-class)))

(defmulti pitch
  (fn [& args]
    (let [[a b] args]
      (cond
        (number? a)     :val
        (map? a)        :map
        (pitch-name? a) :name
        (and (pitch-class-name? a)
             (number? b)) 
          [:pitch-class :octave]))))

(defmethod pitch :val [v] (map->Pitch (val->pitch v)))

(defmethod pitch :name [n] (map->Pitch (name->pitch n)))

(defmethod pitch :map [m]
  (map->Pitch (first-where m pitches)))

(defmethod pitch [:pitch-class :octave] [p o]
  (map->Pitch (first-where {:pitch-class p :octave o} pitches)))


