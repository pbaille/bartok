(ns bartok.rythmn.pattern
  (:use [bartok.rythmn rval utils])
  (:use bartok.structure)
  (:use [clojure.math.combinatorics :as c])
  (:use [utils utils prob dom-part macros]))



(defnaults rythmic-cells
 "returns all [RVal] of a certain size at position start-pos of duration dur"
 [rvals _ 
  size  _ 
  dur   _
  start-pos (g-pos)
  part-results (allowed-rvals start-pos rvals)] 
    (let [allowed-rvals (allowed-rvals start-pos rvals)]
      ))




