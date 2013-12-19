(ns bartok.melody.strategies
  (:use [bartok.types]))

(def ^:private default-prob-map 
  (apply hash-map 
    (mapcat #(list % 1) (keys generic->interval-classes))))

(defn interval-prob-line [interval-prob-map length]
  nil)