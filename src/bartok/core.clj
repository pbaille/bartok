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
 
 (:use [bartok.structure.position])
 
 (:use bartok.composition.rythmic-step-pattern)
 (:use bartok.composition.utils))

(grid {:bars [[7 :4|4]] 
       :tempo [[0 16 120]] 
       :harmony {[0 0] :C-Lyd+
                 [1 0] :Ab-Lyd+
                 [2 0] :Eb-Lyd+
                 [3 0] :B-Lyd+
                 [4 0] :A-Lyd
                 [5 0] :F-Lyd+
                 [6 0] :F-Melm}})

(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{3} 
               :iterations #{2 3 4 5} 
               :steps #{ -4 -3 -1 1 3 4 }
               :cycle-steps #{-3 -2 -1 1 2 3}}))

(def notes 
  (rythmic-step-pattern 
    {:picker picker 
     :rvals [1/4] 
     :start-pos (g-pos 0 0 0) 
     :end-pos (g-pos 2 0 0 )
     :bounds [(b> :C0)(b> :C2)] 
     :start-pitch (b> :C1)}))

(def chords 
  (loop-line 2
    (ap note-line-from (g-pos 0 0 0) 4 
     (map #(a p-chord %) 
      [[:C-1 :M7-u :M3-u1 :+5-u1]
       [:C-1 :P5-u :m6-u :M2-u1 :M3-u1]
       [:C-1 :M6-u :M7-u :M2-u1 :m3-u1 :P5-u1]
       [:B-2 :M7-u :M3-u1 :+5-u1]
       [:B-2 :P5-u :m7-u :M2-u :P4-u]
       [:A-2 :m6-u :M3-u1 :P5-u1 :m7-u1 :M2-u2]
       [:F-1 :P5-u :M2-u1 :m3-u1 :M7-u1]]))))

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn go [] (play vep (concat chords notes)))



  



  