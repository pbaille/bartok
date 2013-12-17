(ns bartok.mode
  (:use [bartok.litterals.identity])
  (:use [bartok.interval-class])
  (:use [bartok.pitch-class])
  (:use [bartok.pitch])
  (:use [bartok.mode-class])
  (:use [bartok.protocols])
  (:use [utils.utils]))

(defrecord Mode [name root mode-class pitch-classes]
  Transpose
  (transpose [this interval]
    (let [r (transpose root interval)
          n (keyword-cat (:name r) "-" (:name mode-class))
          ps (map #(transpose % interval) pitch-classes)]
      (->Mode n r mode-class ps))))


;; type check
(defn mode? [x] (instance? Mode x))

;********* helpers *********

(defn- pitch-classes-calc [root degrees]
  (cons root (map #(transpose root %) degrees)))

;************ construct **************

(defn mode-constructor-dispatch 
  ([] "no args")
  ([a]
   (cond 
     (mode-name? a) :name))
  ([a b] 
   (cond
     (and (mode-name? a) (number? b))
       [:mother-name :degree]))
  ([a b c] "one args"))

(defmulti mode mode-constructor-dispatch )

(defmethod mode :name [n]
  (let [[r mc] (split-mode-name n)
         r (pitch-class r)
         mc (mode-class mc)
         pcs (pitch-classes-calc r (:degrees mc))]
    (->Mode n r mc pcs)))

(defmethod mode [:mother-name :degree] [mn d]
  (let [mc (mode-class (second (split-mode-name mn)) d)
        r (nth (:pitch-classes (mode mn)) (dec d))
        n (keyword-cat (:name r) "-" (:name mc))
        pcs (pitch-classes-calc r (:degrees mc))]
    (->Mode n r mc pcs)))

;************* methods *************

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




