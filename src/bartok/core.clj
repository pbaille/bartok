(ns bartok.core
  
 (:use [bartok.midi.overtone-midi])
 (:use [bartok.midi.midi])
 (:use [utils.utils])
 (:use [utils.prob])
 (:use [clojure.math.combinatorics :as c])
 (:use [utils.interpolator])
 (:use [utils.macros])
 (:use clojure.inspector)
 (:use vendors.debug-repl)
 
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
 
 (:use [bartok.structure.position])
 
 (:use bartok.composition.rythmic-step-pattern)
 (:use bartok.composition.utils))

;***********************************************************

(grid {:bars [[2 :4|4]] 
       :tempo [[0 24 120][26 115]] 
       :harmony {[0 0] :C-Lyd
                 [1 0] :Ab-Lyd}})

                 ; [2 0] :Eb-Lyd+
                 ; [3 0] :B-Lyd+
                 ; [4 0] :A-Lyd
                 ; [5 0] :F-Lyd
                 ; [6 0] :F-Melm
                 
(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{4 3 5 6 7 8} 
               :iterations    #{1 2 3 4} 
               :steps         #{-4 -3 -1 1 3 4}
               :cycle-steps   #{-3 -2 -1 1 2 3}}))

(def notes 
  (rythmic-step-pattern 
    {:picker picker 
     :rvals [1/4] 
     :start-pos (g-pos 0 0 0) 
     :end-pos (g-pos 8 0 0 )
     :bounds [:C0 :C2] 
     :start-pitch :C1 }))

; (def notes 
;   (rythmic-prob-step
;     {:prob-map {:4th-u 0.4
;                 :4th-d 0.4
;                 :5th-u 0.4
;                 :5th-d 0.4}
;      :rvals [1/4] 
;      :start-pos (g-pos 0 0 0) 
;      :end-pos (g-pos 8 0 0 )
;      :bounds [:C0 :A2] 
;      :start-pitch :C1 }))

; (pp (map #(map (c :name :pitch) %) (partition 8 8 notes)))

(def notes (map #(assoc % :velocity (rand-int-between 50 80)) notes))

(def chords 
  (loop-line 8
    (ap m-note-line-from 
     (g-pos 0 0 0) 4 40 1
     (map #(a p-chord %) 
      [[:C-1 :P5 :M6 :M2-u1 :+4-u1]
       [:Ab-2 :P5 :M6 :M2-u1 :+4-u1]
       ; [:C-1 :M6 :M7 :M2-u1 :m3-u1 :P5-u1]
       ; [:B-2 :M7 :M3-u1 :+5-u1]
       ; [:B-2 :P4 :m7 :M2-u1 :M3-u1]
       ; [:A-2 :m6 :m3-u1 :P5-u1 :m7-u1 :M2-u2]
       ; [:F-1 :P5 :M2-u1 :m3-u1 :M7-u1]
       ]))))

; (def basses (loop-line 2 (ap m-note-line-from (g-pos 0 0 0) 4 60 1
;                              [:C-2 :C-2 :C-2 :B-3 :B-3 :A-3 :F-2])))

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn go [] (play vep (concat chords notes)))

;************** perlin line ****************

(import '(render.Noise))

(defn lazy-perlin1 [start step len min-out max-out]
  (let [line (map #(. render.Noise noise %) 
             (take len (iterate (p + step) start)))
        ma (a max line)
        mi (a min line)]
    ; (debug-repl)
    (pev (map #(-> % (scale-range mi ma min-out max-out) int) line))))

(defn play-perlin-line1 [start step len min-out max-out]
  (play vep 
    (ap m-note-line-from 
     (g-pos 0 0 0) 1/4 60 1
     (map pitch (lazy-perlin1 start step len min-out max-out)))))


