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
 (:use [bartok.rythmn.analysis])
 
 (:use [bartok.structure.position]))

(b-def bars [:4|4 :4|4 :4|4 :4|4 :4|4 :4|4 ])

(def g (grid {:bars bars 
              :tempo [[0 16 140]] 
              :harmony [{:position [0 0] :mode (b> :C-Lyd)}
                        {:position [1 0] :mode (b> :Ab-Lyd)}
                        {:position [2 0] :mode (b> :Eb-Lyd)}
                        {:position [3 0] :mode (b> :B-Lyd)}
                        {:position [4 0] :mode (b> :A-Lyd)}
                        {:position [5 0] :mode (b> :F-Lyd)}
                        ]}))

(def g-pos (partial position g))

(def picker (lazy-step-pattern-picker {:cycle-lengths #{3 4 5 6} 
                                       :iterations #{2 3 4} 
                                       :steps #{ -3 -2 -1 1 2 3}
                                       :cycle-steps #{-3 -2 -1 1 2 3}}))

;****************************************************************************

(defn sp-mel [options-map]
  (let [{:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line start-pos rvals start-pos end-pos)
        global-bounds (global-bounds start-pos end-pos bounds start-pitch)
        steps (take (count rl) (steps-line global-bounds picker))
        rl-steps (map #(assoc %1 :step %2) rl steps)
        harmonic-chunks 
          (group-by (juxt #(-> % :position (dissoc :grid) vals vec)
                          #(:name (mode-at (position-add (:position %) (- (:duration %) 1/100))))) 
                    rl-steps)
        harmonic-chunks (sort-by #(-> % first first) harmonic-chunks)  
        pitches (reduce
                  (fn [acc [[_ mode-name] elems]]
                    (let [md (melodic-domain mode-name bounds (or (last acc) start-pitch))
                          s (step-sequence md (map :step elems))]
                      (concat acc s))) 
                  [] harmonic-chunks)]
    (map #(note %2 (:duration %1) (:position %1)) rl pitches)))

(defn notes [] 
  (sp-mel {:picker picker 
           :rvals [1/2] 
           :start-pos (g-pos 0 0 0) 
           :end-pos (g-pos 1 0 0 )
           :bounds [(b> :C0)(b> :C2)] 
           :start-pitch (b> :C1)}))


(p (partition 8 8 (map #(-> % :pitch :name) (notes))))
(p (map #(-> % :position (dissoc :grid)) (sort-by #(-> % :position position-val) (notes))))

(b-fn make-chord [p & gis]
  (reduce #(conj %1 (transpose p %2)) [p] gis))

;(make-chord :C#1 :m2-u :P4-u :P5-u :m7-u)

(b-fn note-line-from [pos dur & notes-and-chords]
  (reduce #(let [l (last %1)
                 l-pos (cond (nil? l) (do (p "prout") (position-add pos (- dur)))
                             (vector? l) (-> l last :position) 
                             :else (:position l))
                 next-pos (position-add  l-pos dur)]
             (conj %1 (if (type= %2 'Pitch)
                        (note %2 dur next-pos)
                        (chord %2 dur next-pos)))) 
          [] notes-and-chords))

;pitch vectors represent chords
;(note-line-from (g-pos 0 0 0) 1 [:C#1 :D2] :C#0 :A2 [:C#1 :E1])

(defn- flat-line-chords [line]
  (mapcat vec-if-not line))

(def chords (flat-line-chords (note-line-from (g-pos 0 0 0) 4 [:C-1 :G-1 :A-1]
                                                              [:Ab-2 :Eb-1 :F-1]
                                                              [:Eb-1 :Bb-1 :F0]
                                                              [:B-2 :F#-1 :C#-1]
                                                              [:A-2 :E-1 :B-1]
                                                              [:F-1 :C0 :G0])))

(declare basses)
;temp print helper
(defn grissoc [coll]
  (map #(if (type= % 'Note)
          (dissoc-in % [:position :grid])
          (map grissoc %)) coll))
  
(defn loop-line [line n]
  (let [end-pos-val (position-val (position-add (:position (last line))(:duration (last line))))]
    (mapcat (fn [[n line]]
              (map #(update-in % [:position] position-add (* n end-pos-val)) line)) 
            (for [nn (range n)] [nn line]))))
;****************************************************************************

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main [& args]
  (def player (partial play-new vep))
  (player (loop-line (concat chords (notes)) 4)))

  
  
  