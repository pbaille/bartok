(ns bartok.core

 (:use [clojure.math.combinatorics :as c])
 (:use clojure.inspector)
 (:use [vendors debug-repl profile])

 (:use utils.all)
 (:use bartok.litterals.all)
 (:use bartok.melody.all)
 
 
 (:use [bartok.midi overtone-midi midi])
 (:use [bartok multimethods note])
 (:use [bartok.harmony harmony h-function])
 (:use [bartok.rythmn rval random-line weighted-line analysis])
 (:use [bartok.structure position])
 (:use [bartok.composition utils rythmic-step-pattern]))

(immigrate 'vendors.perlin-noise)


; (:use [bartok.rythmn.humanize])
;***********************************************************
