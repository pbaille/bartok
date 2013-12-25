(ns bartok.litterals.evaluation
  (:use [bartok.litterals.identity])
  (:use [bartok.litterals.patterns])
  (:use [utils.utils])
  (:use [bartok.types]))

(defn b> 
  ([x] (when-let [t (b-type x)] (call (name t) x)))
  ([x & xs] (map b> (cons x xs))))












