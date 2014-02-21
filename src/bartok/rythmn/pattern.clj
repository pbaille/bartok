(ns bartok.rythmn.pattern
  (:use [bartok.rythmn rval utils])
  (:use bartok.structure)
  (:use bartok.primitives)
  (:use [clojure.math.combinatorics :as c])
  (:use [utils utils prob dom-part macros]))

(defnaults rythmic-cells
 "returns a lazy-seq of [RVal] of a certain size at position start-pos of duration dur
 ex: (rythmic-cells [1/2 1/3 1] 4 2)
 => ([1/2 1/2 1/2 1/2] [1/3 1/3 1/3 1] [1 1/3 1/3 1/3])"
 [rvals _ 
  size  _ 
  dur   _
  start-pos (g-pos)
  heads [[]]] 
    (if (count= (first heads) size)
      ;final filter that remove wrong sum results
      (filter #(= (a + %) dur) heads)
      ;main recursive stuff
      (rythmic-cells rvals size dur start-pos 
        (mapcat 
          (fn [head]
            (let [head-dur (a + head)
                  current-pos (pos+ start-pos head-dur)
                  rvals (filter #(<= (+ head-dur %) dur) rvals)
                  allowed-rvals (shuffle (allowed-rvals current-pos rvals))]
              (map (p conj head) allowed-rvals)))
          heads))))

(defn rand-rythmic-cell 
  "same as rythmic-cells but just pick one"
  [& args] 
  (first (a rythmic-cells args)))

;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;

; (defrecord Aze [name val]
;   clojure.lang.Named
;   (getName [this] (subs (str (:name this)) 1))
;   java.util.Map$Entry
;   (getValue [this] (:val this))
;   (getKey [this] (:name this)))

; (def aze (Aze. :aze 1))

; (name aze)
; (val aze)
; (key aze)
; (name "azeaze")
