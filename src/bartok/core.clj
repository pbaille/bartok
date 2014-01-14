(ns bartok.core

 (:use [clojure.math.combinatorics :as c])
 (:use clojure.inspector)
 (:use vendors.debug-repl)

 (:use [utils utils prob interpolator macros noise])
 (:use [bartok.midi overtone-midi midi])
 (:use [bartok multimethods types note])
 (:use [bartok.litterals identity evaluation])
 (:use [bartok.melody melodic-domain strategies step-pattern])
 (:use [bartok.harmony harmony h-function])
 (:use [bartok.rythmn rval random-line weighted-line analysis])
 (:use  bartok.structure.position)
 (:use [bartok.composition utils rythmic-step-pattern]))

; (:use [bartok.rythmn.humanize])
;***********************************************************


