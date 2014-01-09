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

(grid {:bars [[7 :4|4]] 
       :tempo [[0 16 120]] 
       :harmony {[0 0] :C-Lyd+
                 [1 0] :Ab-Lyd+
                 [2 0] :Eb-Lyd+
                 [3 0] :B-Lyd+
                 [4 0] :A-Lyd
                 [5 0] :F-Lyd+
                 [6 0] :F-Melm}})

; (def g-pos (p position g))

(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{2 3 4} 
               :iterations #{2 3 4} 
               :steps #{ -4 -3 -1 1 3 4 }
               :cycle-steps #{-3 -2 -1 1 2 3}}))

;****************************************************************************

(defn sp-mel [options-map]
  (let [{:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line start-pos rvals start-pos end-pos)
        global-bounds (global-bounds start-pos end-pos bounds start-pitch)
        steps (take (count rl) (steps-line global-bounds picker))
        harmonic-chunks 
          (map #(assoc % :position (-> % :position (dissoc :grid) vals vec)
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

(defn notes [] 
  (sp-mel {:picker picker 
           :rvals [1/4] 
           :start-pos (g-pos 0 0 0) 
           :end-pos (g-pos 2 0 0 )
           :bounds [(b> :C0)(b> :C2)] 
           :start-pitch (b> :C1)}))

; (dorun (map #(println %) (partition 16 16 (map #(-> % :pitch :name) (notes)))))

(b-fn make-chord [p & gis]
  (reduce #(conj %1 (transpose p %2)) [p] gis))

;(make-chord :C#1 :m2-u :P4-u :P5-u :m7-u)

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

(defn- flat-line-chords [line]
  (mapcat vec-if-not line))

(def make-lyd-chord
  #(make-chord % :M6-u :M2-u1 :+4-u1))

(def make-lyd+-chord
  #(make-chord % :M7-u :M3-u1 :+5-u1))

(def chords 
  (map #(assoc % :velocity 40)
       (flat-line-chords 
         (note-line-from (g-pos 0 0 0) 4 
           (make-lyd+-chord :C-1)              
           (make-chord :C-1 :P5-u :m6-u :M2-u1 :M3-u1)              
           (make-chord :C-1 :M6-u :M7-u :M2-u1 :m3-u1 :P5-u1)              
           (make-lyd+-chord :B-2)              
           (make-chord :B-2 :P5-u :m7-u :M2-u :P4-u)
           (make-chord :A-2 :m6-u :M3-u1 :P5-u1 :m7-u1 :M2-u2)              
           (make-chord :F-1 :P5-u :M2-u1 :m3-u1 :M7-u1)))))

(declare basses)
;temp print helper
(defn grissoc [coll]
  (map #(if (type= % 'Note)
          (dissoc-in % [:position :grid])
          (map grissoc %)) coll))
  
(defn loop-line [line n]
  (let [end-pos-val (pos-val (pos+ (:position (last line))(:duration (last line))))]
    (mapcat (fn [[n line]]
              (map #(update-in % [:position] pos+ (* n end-pos-val)) line)) 
            (for [nn (range n)] [nn line]))))

;****************************************************************************

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main [& args]
  (def player (partial play-new vep))
  (player (concat (loop-line chords 2) (notes))))



  



  