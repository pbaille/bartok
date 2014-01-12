(ns bartok.rythmn.humanize
  (:use utils.utils)
  (:use bartok.structure.position)
  (:use utils.prob)
  (:use utils.interpolator))


(defn humanize-tempo [grid rate delt-bpm motion detail]
  (let [points (map (juxt identity tempo-at) (range-by (cycle-val) rate))
        detail-div #(int (/ % detail))
        drunk-line (drunk-sequencer2 [(- delt-bpm) delt-bpm] motion 0 false detail)
        delts (take (count points) (drunk-line))]
    (map (fn [[a b] d] [a (+ b d)]) points delts)))