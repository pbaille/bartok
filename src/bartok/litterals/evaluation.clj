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
;       (pitch-class-kw? x) (pitch-class x)
;       (pitch-kw? x)       (pitch x)
;       (interval-kw? x)    (interval x)
;       (m-degree-kw? x)    (m-degree x)
;       (abs-mode-kw? x)    (abs-mode x)
;       (mode-kw? x)        (mode x))))