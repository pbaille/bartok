(ns bartok.multimethods
  (:use [bartok.litterals.identity]))

(defmulti transpose b-type )

