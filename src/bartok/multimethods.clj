(ns bartok.multimethods
  (:use [bartok.litterals.identity]))

(defmulti transpose b-types)
(defmulti distance b-types)


