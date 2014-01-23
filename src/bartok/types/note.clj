(ns bartok.types.note
  (:use utils.utils)
  (:use bartok.primitives))

(b-fn note 
  ([p dur pos]
    (with-type 'Note {:pitch (pitch p) :duration dur :position pos}))
  ([p dur pos vel chan]
    (with-type 'Note {:pitch (pitch p) :duration dur :position pos :velocity vel :channel chan})))

(b-fn p-chord 
  [pitch & intervals-and-or-degrees]
  (with-type 'PChord (reduce #(conj %1 (transpose pitch %2)) [pitch] 
                             (map c-interval intervals-and-or-degrees))))

;(p-chord :C#1 :m2-u :P4-u :P5-u :m7-u)
;(p-chord :C-1 :P4 :M6 :m7 :M2-u1 :M3-u1)

(b-fn chord [pitches dur pos]
  (with-type 'Chord 
    {:pitches pitches :duration dur :position pos}))