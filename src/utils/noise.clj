(ns utils.noise
  (:import clisk.noise.Perlin)
  (:import clisk.noise.Simplex)
  (:use utils.utils))

;(:import (render.Noise))

; (defn noise 
;   ([a](render.Noise/noise a))
;   ([a b](render.Noise/noise a b))
;   ([a b c](render.Noise/noise a b c)))

;*************** perlin ****************

(defn pnoise 
  ([a](clisk.noise.Perlin/noise a))
  ([a b](clisk.noise.Perlin/noise a b))
  ([a b c](clisk.noise.Perlin/noise a b c)))

(defn perlin-line [steps]
  (let [seeds (take (count steps) (repeatedly #(+ (rand-int 1e5) (rand))))]
    (a map pnoise (map #(iterate (p + %2) %1) seeds steps))))

(defn scaled-perlin 
  [steps min-out max-out len]
  (let [line (take len (perlin-line steps))
       [mi ma] ((juxt #(a min %) #(a max %)) line)]
    (map (c int (range-scaler mi ma min-out max-out)) 
         line)))

;*************** simplex ****************

(defn snoise 
  ([a b](clisk.noise.Simplex/noise a b))
  ([a b c](clisk.noise.Simplex/noise a b c))
  ([a b c d](clisk.noise.Simplex/noise a b c d)))

(defn simplex-line [steps]
  (let [seeds (take (count steps) (repeatedly #(+ (rand-int 1e5) (rand))))]
    (a map snoise (map #(iterate (p + %2) %1) seeds steps))))

(defn scaled-simplex 
  [steps min-out max-out len]
  (let [line (take len (simplex-line steps))
       [mi ma] ((juxt #(a min %) #(a max %)) line)]
    (map (c int (range-scaler mi ma min-out max-out)) 
         line)))

;******************* examples ******************

; (defn play-perlin-line [steps md len]
;   (play vep 
;     (m-note-line-from 
;      (g-pos 0 0 0) 1/4 60 1
;      (map #(nth (:pitches md) %)
;           (scaled-perlin steps 0 (dec (md-amplitude md)) len)))))

; (defn play-simplex-line [steps md len]
;   (play vep 
;     (m-note-line-from 
;      (g-pos 0 0 0) 1/4 60 1
;      (map #(nth (:pitches md) %)
;           (scaled-simplex steps 0 (dec (md-amplitude md)) len)))))
