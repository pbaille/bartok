(ns bartok.core
 (:use [midi])
 (:use [bartok.midi-fns])
 (:use [utils.utils])
 (:use [utils.macros])
 (:use clojure.inspector)
 
 (:use [bartok.multimethods])
 (:use [bartok.types])
 
 (:use [bartok.melody])
 (:use [bartok.melody.strategies])
 (:use [bartok.melody.step-pattern])
 
 (:use [bartok.harmony.harmony])
 (:use [bartok.harmony.h-function])
 
 (:use [bartok.rythmn.rval])
 (:use [bartok.rythmn.random-line])
 
 (:use [bartok.structure.grid])
 
 (:use [bartok.litterals.identity])
 (:use [bartok.litterals.evaluation]))

(b-def bars [:4|4 :4|4 :3|4 :3|4])






;****************************************************************************

; (def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

; (defn -main

;   [& args]

;   (def player (partial play-line vep ))
;   (def line (map #(vector %1 (rand-int-between 40 90) %2)
;                   (make-drunk-line [50 80] 7 50 60)
;                   (map #(to-ms % 60) (r-line [1/2 1/3 1/4] 0))))
;   (apply player line))

