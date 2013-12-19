(in-ns 'bartok.types)

(load "types/interval_class")
(load "types/interval")
(load "types/pitch_class")
(load "types/pitch")
(load "types/mode_class")

; (defrecord Mode [name root mode-class pitch-classes]
;   Transpose
;   (transpose [this interval]
;     (let [r (transpose root interval)
;           n (keyword-cat (:name r) "-" (:name mode-class))
;           ps (map #(transpose % interval) pitch-classes)]
;       (->Mode n r mode-class ps))))


;; type check
;(defn mode? [x] (instance? Mode x))

;********* helpers *********

(defn- pitch-classes-calc [root degrees]
  (cons root (map #(transpose root %) degrees)))

(defn build-mode [n r mc pcs]
  (with-type 'Mode {:name n :root r :mode-class mc :pitch-classes pcs}))

;************ construct **************

(defmulti mode b-type )

(defmethod mode :mode [n]
  (let [[r mc] (split-mode-name n)
         r (pitch-class r)
         mc (mode-class mc)
         pcs (pitch-classes-calc r (:degrees mc))]
    (build-mode n r mc pcs)))

(defmethod mode [:mode :number] [mn d]
  (let [mc (mode-class (second (split-mode-name mn)) d)
        r (nth (:pitch-classes (mode mn)) (dec d))
        n (keyword-cat (:name r) "-" (:name mc))
        pcs (pitch-classes-calc r (:degrees mc))]
    (build-mode n r mc pcs)))

;************* methods *************

; (defmethod transpose 'Mode [this interval]
;   (let [r (transpose (:root this) interval)
;         n (keyword-cat (:name r) "-" (:name mode-class))
;         ps (map #(transpose % interval) (:pitch-classes this))]
;     (build-mode n r (:mode-class this) ps))))

(defn mother-root [m]
  (let [d (get-in m [:mode-class :degree])]
    (first (rotate (:pitch-classes m) (* -1 (dec d))))))

(defn mother-mode [m]
  (mode (keyword-cat (:name (mother-root m)) "-" (get-in m [:mode-class :mother]))))

(defn relative [m new-mode-class]
  (let [nmc (mode-class new-mode-class)
        degree (:degree nmc)
        mother-mode-name (keyword-cat (:name (mother-root m)) "-" (:mother nmc))]
    (mode mother-mode-name degree)))

(defn intra-abs-move [m n]
  (mode (:name(mother-mode m)) n))

(defn intra-rel-move [m n]
  (mode (:name(mother-mode m)) 
        (#(inc (mod % 7)) (+ 7 (-> m :mode-class :degree dec) n))))




