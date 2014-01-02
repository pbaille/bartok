(ns bartok.note
  (:use utils.utils)
  (:use bartok.litterals.evaluation))

(b-fn note [pitch dur pos]
  (with-type 'Note {:pitch pitch :duration dur :position pos}))