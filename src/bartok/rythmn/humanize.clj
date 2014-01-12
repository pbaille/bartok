(ns bartok.rythmn.humanize
  (:use utils.utils)
  (:use bartok.structure.position)
  (:use utils.prob)
  (:use utils.interpolator))


(defn humanize-tempo [grid rate delt-bpm motion detail]
  (let [points (map (juxt identity tempo-at) (range-by (cycle-val) rate))
        detail-div #(int (/ % detail))
        drunk-line (drunk-sequencer [(detail-div (- delt-bpm)) 
                                     (detail-div delt-bpm)] 
                                    (detail-div motion))
        delts (take (count points) (drunk-line))]
    (map #(vector (first %) (+ (second %) (* detail %2))) points delts)))