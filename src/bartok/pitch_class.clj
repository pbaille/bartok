(ns bartok.pitch-class
  (:use [bartok.constants])
  (:use [utils.utils]))

(defprotocol IPitchClass
  (unaltered-name [this])
  (alt-name [this])
  (alt-val [this]))

(defrecord PitchClass [name c-dist]
  IPitchClass
  (unaltered-name [this] (subs name 0 1))
  (alt-name [this] (subs name 1))
  (alt-val [this] (alterations-values (alt-name this))))

;********** Constructor **************

(defmulti pitch-class
  (fn [& args]
    (cond
      (count= args 1)
        (if (or (keyword? (first args)) (string? (first args))) 
            :name
            :integer)
      (count= args 2) 
        (if (and (or (keyword? (first args)) (string? (first args)))
                 (number? (second args)))
            [:unaltered-name :c-dist]))))


(defmethod pitch-class :name
  [n]
  (let [name-str (name n)
        unaltered-name (subs name-str 0 1)
        alt-name (subs name-str 1)
        alt-val  (or (alterations-values alt-name) 0)
        c-dist (+ (unaltered-pitch-classes (keyword unaltered-name)) alt-val)]
    (->PitchClass name-str c-dist)))

(defmethod pitch-class :integer [i] 
  (let [n (pitch-class-defaults-names i)] (pitch-class n)))

(defmethod pitch-class [:unaltered-name :c-dist]
  [un cd]
  (let [diff (- cd (unaltered-pitch-classes (keyword un)))
        alt-val (cond (< 2 diff) (- diff 12) (> -2 diff) (+ diff 12) :else diff)
        alt-name (alterations-names alt-val)
        name-str (str (name un) alt-name)]
    (->PitchClass name-str cd)))

;*************** Methods *****************

(defn pitch-class? [this] (= (class this) rand_music.pitch_class.PitchClass))





