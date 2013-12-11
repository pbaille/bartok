; (ns bartok.litterals.evaluation
;   (:use [bartok.litterals.identity])
;   (:use [bartok.constants])
;   (:use [bartok.pitch-class])
;   (:use [bartok.pitch])
;   (:use [bartok.interval])
;   (:use [bartok.m-degree])
;   (:use [bartok.mode])
;   (:use [utils.utils]))

; (defn bk [x]
;   (when (keyword? x)
;     (cond
;       (pitch-class-name? x) (pitch-class x)
;       (pitch-name? x)       (pitch x)
;       (interval-name? x)    (interval x)
;       (m-degree-name? x)    (m-degree x)
;       (abs-mode-name? x)    (abs-mode x)
;       (mode-name? x)        (mode x))))