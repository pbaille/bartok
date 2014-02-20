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
  heads [[]]] 
    (if (count= (first heads) size)
      (filter #(= (a + %) dur) heads)
      (rythmic-cells rvals size dur start-pos 
        (mapcat 
          (fn [head]
            (let [head-dur (a + head)
                  current-pos (pos+ start-pos head-dur)
                  rvals (filter #(<= (+ head-dur %) dur) rvals)
                  allowed-rvals (allowed-rvals current-pos rvals)]
              (map (p conj head) allowed-rvals)))
          heads))))


