(ns bartok.protocols)

(defprotocol Transpose
  (transpose [this of]))

; (defprotocol Casting
;   (pitch-class [this])
;   (pitch [this])
;   (interval-class [this])
;   (interval [this])
;   (mode-class [this])
;   (mode [this]))
