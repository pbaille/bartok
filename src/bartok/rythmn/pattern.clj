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

(defn- constrain-to-bounds [x [mi ma]]
  (cond (< x mi) mi (> x ma) ma :else x))

(defn- r-cell-size-bounds [rvals len]
  [(int (m/floor (/ len (a max rvals))))
   (int (m/ceil (/ len (a min rvals))))])

(defn- r-cell-median-size [rvals len]
  (median (map (p / len) rvals)))

(defn- density-range-scaler [rvals len]
  (let [med (r-cell-median-size rvals len)
        [mis mas] (r-cell-size-bounds rvals len)]
    #(cond 
      (<= 0.5 % 1) (m/round (scale-range % 0.5 1 med mas))
      (>= 0.5 % 0) (m/round (scale-range % 0 0.5 mis med))
      :else (if (< % 0) mis mas))))

;statefull and not really random, have to do better
(defn rythmic-cell-picker 
  [{:keys [rvals lengths density] :as options}]
  (let [cnt (count lengths)
        ;keep track of current index in each r-cell-seq
        idxs (map (fn [x] (atom 0)) (range cnt))
        r-cells-seqs
        (map #(rythmic-cells 
                  rvals 
                  ((density-range-scaler rvals %) density) 
                  %) 
             lengths)]
    ;pick the current index of a r-cells-seq then inc idx tracker 
    #(let [x (rand-int cnt)
           coll (nth r-cells-seqs x)
           idx (nth idxs x)]
       (if-let [ret (nth coll @idx nil)]
         (do (swap! idx inc) ret)
         (do (reset! idx 1) (first coll))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defn rvals-dom-part [rvals density length]
;   (let [combs (dom-part 
;                 rvals 
;                 ((density-range-scaler rvals length) density) 
;                 length)]))

;;;;;;;;;;;;;;;;;;;; skull ;;;;;;;;;;;;;;;;;;;;;

