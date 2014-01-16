(ns bartok.core

 (:use [clojure.math.combinatorics :as c])
 (:use clojure.inspector)
 (:use [vendors debug-repl profile])
 
 (:use bartok.multimethods)
 (:use bartok.types.note)
 
 (:use [bartok.midi overtone-midi midi parser])
 (:use [bartok.structure position])
 
 (:use utils.all)
 (:use bartok.litterals.all)
 (:use bartok.melody.all)
 (:use bartok.harmony.all)
 (:use bartok.rythmn.all)
 (:use bartok.composition.all))


; (:use [bartok.rythmn.humanize])
