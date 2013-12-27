(ns bartok.multimethods
  (:use [bartok.litterals.identity]))

(defmulti transpose b-types)

;modal-moves
(defmulti intra-abs-move b-types)
(defmulti intra-rel-move b-types)
(defmulti relative b-types)


