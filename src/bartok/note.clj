(ns bartok.note
  (:use utils.utils)
  (:use bartok.multimethods)
  (:use bartok.types)
  (:use bartok.litterals.evaluation))

(b-fn note [pitch dur pos]
  (with-type 'Note {:pitch pitch :duration dur :position pos}))

(b-fn chord [pitches dur pos]
  (with-type 'Chord (mapv #(note % dur pos) pitches)))

(b-fn p-chord [pitch & gis]
  (with-type 'PChord (reduce #(conj %1 (transpose pitch %2)) [pitch] gis)))

;(p-chord :C#1 :m2-u :P4-u :P5-u :m7-u)

(b-fn chord2 [pitches dur pos]
  (with-type 'Chord 
    {:pitches pitches :duration dur :position pos}))