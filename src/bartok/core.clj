(ns bartok.core
  
 (:use [midi])
 (:use [bartok.midi.midi])
 (:use [utils.utils])
 (:use [clojure.math.combinatorics :as c])
 (:use [utils.interpolator])
 (:use [utils.macros])
 (:use clojure.inspector)
 
 (:use [bartok.multimethods])
 (:use [bartok.types])
 
 (:use [bartok.litterals.identity])
 (:use [bartok.litterals.evaluation])
 
 (:use [bartok.note])
 
 (:use [bartok.melody.melodic-domain])
 (:use [bartok.melody.strategies])
 (:use [bartok.melody.step-pattern])
 
 (:use [bartok.harmony.harmony])
 (:use [bartok.harmony.h-function])
 
 (:use [bartok.rythmn.rval])
 (:use [bartok.rythmn.random-line])
 
 (:use [bartok.structure.position]))

(b-def bars [:4|4 :4|4 :4|4 :4|4])

(def g (grid {:bars bars 
              :tempo [[0 16 240]] 
              :harmony [{:position [0 0] :mode (b> :C-Lyd)}
                        {:position [2 0] :mode (b> :Ab-Lyd)}]}))

(def g-pos (partial position g))

(defn harmony-at [pos]
  (:mode (last 
    (filter #(<= (position-val (assoc pos :bar (-> % :position first) :sub (-> % :position second))) 
                 (position-val pos)) 
            (-> pos :grid :harmony)))))

(def n (note :C#1 1/2 (g-pos 0 1 1/2)))

(def ab-lyd (melodic-domain :C-Lyd [:C-1 :G2] :C1))
(def c-lyd  (melodic-domain :Ab-Lyd [:C-1 :G2] :C1))

(def picker (step-pattern-picker {:cycle-lengths #{3 4 5 6} :iterations #{2 3 4}}))

;****************************************************************************

(defn sp-mel [options-map]
  (let [{:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (take-while #(< (position-val (:position %)) 
                           (position-val end-pos)) 
                       (r-line start-pos rvals))
        
        ; have extract this
        modes (set (map #(-> (harmony-at (position-add (:position %) (- (:duration %) 1/100))) :name) rl))
        domains (->> modes (map #(melodic-domain % bounds start-pitch)))
        domains-bounds (map #(map :val (interval-bounds %)) domains)
        global-bounds [(best > (map first domains-bounds)) (best < (map second domains-bounds))]
        
        steps (take (count rl) (steps-line global-bounds picker))
        rl-steps (map #(assoc %1 :step %2) rl steps)
        harmonic-chunks 
          (group-by (juxt #(-> % :position :cycle)
                          #(:name (harmony-at (position-add (:position %) (- (:duration %) 1/100))))) 
                    rl-steps)
        pitches (reduce
                  (fn [acc [[_ mode-name] elems]]
                    (let [md (melodic-domain mode-name bounds (or (last acc) start-pitch))
                          s (step-sequence md (map :step elems))]
                      (concat acc s))) 
                  [] harmonic-chunks)]
    (map #(note %2 (:duration %1) (:position %1)) rl pitches)))

(def notes 
  (sp-mel {:picker picker 
           :rvals [1/2] 
           :start-pos (g-pos 0 0 0) 
           :end-pos (g-pos 3 0 0 )
           :bounds [(b> :C-1)(b> :C2)] 
           :start-pitch (b> :C0)}))

(def line (map #(vector (-> % :pitch :val) 80 (note-to-ms %)) notes ))
  
;****************************************************************************

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main [& args]

  (def player (partial play-line vep ))

  
  (apply player line))

