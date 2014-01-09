(ns bartok.composition.utils
  (:use bartok.structure.position)
  (:use bartok.note)
  (:use utils.utils)
  (:use bartok.litterals.evaluation))

(b-fn note-line-from [pos dur & notes-and-chords]
  (reduce #(let [l (last %1)
                 l-pos (cond (nil? l) (pos- pos dur)
                             (vector? l) (-> l last :position) 
                             :else (:position l))
                 next-pos (pos+  l-pos dur)]
             (conj %1 (if (type= %2 'Pitch)
                        (note %2 dur next-pos)
                        (chord %2 dur next-pos)))) 
          [] notes-and-chords))

;pitch vectors represent chords
;(note-line-from (g-pos 0 0 0) 1 [:C#1 :D2] :C#0 :A2 [:C#1 :E1])

(defn loop-line [n line]
  (let [end-pos-val (pos-val (pos+ (:position (last line))(:duration (last line))))]
    (mapcat (fn [[n line]]
              (map #(update-in % [:position] pos+ (* n end-pos-val)) line)) 
            (for [nn (range n)] [nn line]))))