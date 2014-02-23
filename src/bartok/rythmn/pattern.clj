(ns bartok.rythmn.pattern
  (:use [bartok.rythmn rval utils])
  (:use bartok.structure)
  (:use bartok.primitives)
  (:use [clojure.math.combinatorics :as c])
  (:require [clojure.contrib.math :as m :exclude [abs]])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{:rvals [1 1/2 1/3 2/3 1/4]
 :lengths #{3 4 5 6}
 :density 0.5}

(median [1 1/2 1/3 2/3 1/4])
((range-scaler 0 1 0 2) 0.5)

(defn constrain-to-bounds [x [mi ma]]
  (cond (< x mi) mi (> x ma) ma :else x))

(defn r-cell-size-bounds [rvals len]
  [(int (m/floor (/ len (a max rvals))))
   (int (m/ceil (/ len (a min rvals))))])

(defn r-cell-median-size [rvals len]
  (median (map (p / len) rvals)))

(defn density-range-scaler [rvals len]
  (let [med (r-cell-median-size rvals len)
        [mis mas] (r-cell-size-bounds rvals len)]
    #(cond 
      (<= 0.5 % 1) (m/round (scale-range % 0.5 1 med mas))
      (>= 0.5 % 0) (m/round (scale-range % 0 0.5 mis med))
      :else (if (< % 0) mis mas))))

(defn r-patt-picker 
  [{:keys [rvals lengths density] :as options}]
  (let [r-cells-lazy-seqs
        (map #(rythmic-cells rvals ((density-range-scaler rvals %) density) %) 
             lengths)]
    #()))

; (defn r-patt [options]
;   (let [len-dens-s (map )]))

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
