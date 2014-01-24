(ns bartok.xml.transform
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.types.note)
  (:use bartok.melody.all)
  (:use bartok.structure.position))


(defn- measure-notes [m] 
  (->> m :voices vals (a concat) flatten 
       (map #(note (:pitch %)(:duration %)(:position %)))))

(defn- harmonic-an [m]
  (closest-mothers3 (measure-notes m)))

(defn voice->steps [voice]
  (let [just-pitches (map #(if (vector? %) (map :pitch %) (:pitch %)) voice)
        pitch-line (map #(if (sequential? %) (:name (a highest %)) %) just-pitches)]
    (for [[p1 p2] (partition 2 1 pitch-line)] (c-interval p1 p2))))