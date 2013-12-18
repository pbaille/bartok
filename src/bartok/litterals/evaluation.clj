(ns bartok.litterals.evaluation
  (:use [bartok.litterals.identity])
  (:use [utils.utils])
  (:use [bartok.types]))

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