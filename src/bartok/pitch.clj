(ns bartok.pitch
  (:use [bartok.constants])
  (:use [bartok.litterals.identity])
  (:use [bartok.pitch-class])
  (:use [utils.utils]))

(def pitches 
  (reduce #(if %2 (conj %1 %2) %1) #{}      
    (for [[pcn pcv] pitch-classes 
          oct (range -6 7)]
      (let [nam (keyword (str (name pcn) oct))
            val (+ (* 12 (+ oct 5)) (:val pcv))]
        (when (between val 0 127 )
          {:name nam
           :val val
           :octave oct
           :pitch-class pcv})))))

; (defprotocol Transposable
;   (transpose [this x])
;   )

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

(defmethod pitch :val [v]
  (map->Pitch (->> (select-where {:val v} pitches) 
                   (remove #(#{:x :bb :#} (get-in % [:pitch-class :alteration :name])))
                   first)))

(defmethod pitch :name [n]
  (map->Pitch (first-where {:name n} pitches)))

(defmethod pitch :map [m]
  (map->Pitch (first-where m pitches)))

(defmethod pitch [:pitch-class :octave] [p o]
  (map->Pitch (first-where {:pitch-class p :octave o} pitches)))


