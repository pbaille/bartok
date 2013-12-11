(ns bartok.pitch-class
  (:use [bartok.alteration])
  (:use [bartok.natural-pitch-class])
  (:use [bartok.litterals.identity])
  (:use [utils.utils]))

(def pitch-classes 
  (reduce conj #{}      
    (for [{npcn :name npcv :val} natural-pitch-classes 
          {an :name av :val} alterations]
      {:name (if (= av 0) npcn (keyword-cat npcn an))
       :val (mod (+ npcv av 12) 12)
       :natural (natural-pitch-class npcn)
       :alteration (alteration av)})))

(def pitch-class-defaults-names 
  #{:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B})

(def default-name-pitch-classes 
  (filter pitch-class-defaults-names pitch-classes))

; for performance
(def name->pitch-class (reduce #(into %1 {(:name %2) %2}) {} pitch-classes))
(def val->pitch-class  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitch-classes))

(defrecord PitchClass [name val natural alteration])

; ;*********** Constructor ***********

(defn map->PitchClass [m]
  (let [{:keys [name val natural alteration]} m]
    (->PitchClass name val natural alteration)))

(defmulti pitch-class
  (fn [& args]
    (let [[a b] args]
      (cond
        (number? a) :val
        (map? a) :map
        (pitch-class-name? a) :name))))

(defmethod pitch-class :val [v] (map->PitchClass (val->pitch-class v)))

(defmethod pitch-class :name [n] (map->PitchClass (name->pitch-class n)))

(defmethod pitch-class :map [m] (map->PitchClass (first-where m pitch-classes)))


