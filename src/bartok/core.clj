(ns bartok.core

 (:use [clojure.math.combinatorics :as c])
 (:use clojure.inspector)
 (:use [vendors debug-repl profile])
 (:use camel-snake-kebab)
 (:use midje.sweet)
 
 (:use bartok.types.note)
 
 (:use [bartok.midi overtone-midi midi parser])
 (:use [bartok.xml parser transform analysis])
 
 (:use bartok.state)
 (:use utils.all)
 (:use bartok.primitives)
 (:use bartok.structure)
 (:use bartok.melody.all)
 (:use bartok.harmony.all)
 (:use bartok.rythmn.all)
 (:use bartok.composition.all))


; (:use [bartok.rythmn.humanize])


