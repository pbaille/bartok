(ns bartok.pitch
  (:use [bartok.constants])
  (:use [bartok.pitch-class])
  (:use [utils.utils]))

(declare pitch)

(defprotocol IPitch
  (to-midi-pitch [this])
  (c-dist [this])
  )

(defprotocol Transposable
  (transpose [this n])
  )

(defrecord Pitch [pitch-class octave]
  IPitch
  (to-midi-pitch [_] 
    (+ (:c-dist pitch-class) (* (+ octave 5) 12)))
  (c-dist [_] (:c-dist pitch-class))

  Transposable
  (transpose [this n]
    (pitch (+ n (to-midi-pitch this)))))
  

;*********** Constructor ***********

(defmulti pitch
  (fn [& args]
    (cond 
      (-> args count (= 1))
        (cond 
          (map? (first args))    :map
          (number? (first args)) :midi-pitch
          (or (keyword? (first args)) 
              (string? (first args))) 
            :midi-name)
        
      (-> args count (= 2))
        (cond
          (or (keyword? (first args)) 
              (string? (first args))) 
            [:pitch-class-name :octave]
          (pitch-class? (first args))
            [:pitch-class :octave]))))

(defmethod pitch :midi-pitch [p]
  (let [[d r] (div-mod p 12) 
         d (- d 5)
         pc (pitch-class r)]
    (->Pitch pc d)))

(defmethod pitch :map [m]
  (let [{:keys [pitch-class octave]} m]
    (if (and pitch-class octave)
        (->Pitch pitch-class octave))))

(defmethod pitch :midi-name [n]
  (let [s (if (keyword? n) (kw->str n) n)
        oct (parse-int (re-find #"\-\d|\d" s))
        pc  (first (clojure.string/split s #"\-\d|\d"))]
    (->Pitch (pitch-class (keyword pc)) oct)))

(defmethod pitch [:pitch-class-name :octave] [pcn o] 
  (->Pitch (pitch-class pcn) o))

(defmethod pitch [:pitch-class :octave] [pcn o] 
  (->Pitch pcn o))





