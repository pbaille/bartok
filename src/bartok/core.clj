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

(b-def bars [:4|4 :4|4])
(def g (grid {:bars bars :tempo [[0 240][4 280]]}))
(def g-pos (partial position g))

(def n (note :C#1 1/2 (g-pos 0 1 1/2)))

(def d (melodic-domain :C-Lyd [:C-1 :G2] :C1))

(def picker (step-pattern-picker {:cycle-lengths #{3 4 5 6} :iterations #{2 3 4}}))



;****************************************************************************

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main [& args]

  (def player (partial play-line vep ))
  
  (def pat (map :val (step-sequence d (:sequence (picker d)))))
  
  (def line (map #(vector %1 80 %2) (map :val (take 100 (step-patterns-line d picker))) (map note-to-ms (r-line (g-pos 0 0 0) [1/2]))))
  (apply player line))

