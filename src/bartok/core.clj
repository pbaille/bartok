(ns bartok.core

 (:use [clojure.math.combinatorics :as c])
 (:use clojure.inspector)
 (:use [vendors debug-repl profile])
 (:use camel-snake-kebab)
 (:use midje.sweet)
 
 (:use bartok.types.note)
 
 (:use [bartok.midi overtone-midi midi parser])
 (:use [bartok.structure position])
 
 (:use utils.all)
 (:use bartok.primitives)
 (:use bartok.melody.all)
 (:use bartok.harmony.all)
 (:use bartok.rythmn.all)
 (:use bartok.composition.all))


; (:use [bartok.rythmn.humanize])
(def *settings* (atom
  {:tempo-interpolation :step ;# {:step :linear} TODO curve interpolations
   })) ;


