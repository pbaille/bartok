(ns bartok.litterals.evaluation
  (:use [bartok.litterals.identity])
  (:use [bartok.pitch-class])
  (:use [bartok.pitch])
  (:use [bartok.interval-class])
  (:use [bartok.interval])
  (:use [bartok.mode-class])
  (:use [bartok.mode])
  (:use [utils.utils]))

(defn b> [x]
  (if (keyword? x)
    (cond
      (pitch-class-name? x) (pitch-class x)
      (pitch-name? x) (pitch x)
      (interval-class-name? x) (interval-class x)
      (interval-name? x) (interval x)
      (mode-class-name? x) (mode-class x)
      (mode-name? x) (mode x)
      :else nil)
    x ))