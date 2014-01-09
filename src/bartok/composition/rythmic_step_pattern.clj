(ns bartok.composition.rythmic-step-pattern
  (:use bartok.structure.position)
  (:use bartok.litterals.evaluation)
  (:use bartok.note)
  (:use utils.utils)
  
  (:use [bartok.melody.melodic-domain])
  (:use [bartok.melody.strategies])
  (:use [bartok.melody.step-pattern])
 
  (:use [bartok.harmony.harmony])
  (:use [bartok.harmony.h-function])
 
  (:use [bartok.rythmn.rval])
  (:use [bartok.rythmn.random-line])
  (:use [bartok.rythmn.analysis]))

(defn rythmic-step-pattern [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line start-pos rvals start-pos end-pos)
        global-bounds (global-bounds start-pos end-pos bounds start-pitch)
        steps (take (count rl) (steps-line global-bounds picker))
        harmonic-chunks 
          (map #(assoc % :position (-> % :position vals vec)
                         :mode (:name (mode-at (pos+ (:position %) (- (:duration %) 1/100))))) 
               (map #(assoc %1 :step %2) rl steps)) 
        harmonic-chunks 
          (reduce (fn [acc {mode :mode :as x}]
                    (if (= mode (or (:mode (last acc)) nil))
                      (conj (vec (butlast acc)) 
                            (update-in (last acc) [:steps] conj (:step x)))
                      (conj acc {:mode mode :steps [(:step x)]}))) 
                  [] (sort-by :position harmonic-chunks))
        pitches (step-sequence harmonic-chunks bounds start-pitch)]
    (map #(note %2 (:duration %1) (:position %1)) rl pitches)))