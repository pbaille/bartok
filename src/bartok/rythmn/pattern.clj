(ns bartok.rythmn.pattern
  (:use [bartok.rythmn rval utils])
  (:use bartok.structure)
  (:use [clojure.math.combinatorics :as c])
  (:use [utils utils prob dom-part macros]))

(defnaults rythmic-cells
 "returns all [RVal] of a certain size at position start-pos of duration dur"
 ([rvals size dur] 
  "default to start-pos 0"
  (rythmic-cells rvals size dur (g-pos)))
 ([rvals size dur start-pos]
  (let [allowed-rvals (allowed-rvals start-pos rvals)])))




