(ns bartok.mode
  (:use [bartok.litterals.identity])
  (:use [bartok.interval-class])
  (:use [bartok.natural-pitch-class])
  (:use [bartok.pitch-class])
  (:use [bartok.pitch])
  (:use [bartok.mode-class])
  (:use [utils.utils]))

(defrecord Mode [name root mode-class pitch-classes])

;********* helpers *********

(defn- pitch-classes-calc [root degrees]
  (map #(transpose root %) degrees))

;************ construct **************

(defn mode-constructor-dispatch 
  ([] "no args")
  ([a]
   (cond 
     (mode-name? a) :name))
  ([a b] "one args")
  ([a b c] "one args"))

(defmulti mode mode-constructor-dispatch )

(defmethod mode :name [n]
  
  (let [[r mc] (split-mode-name n)
         r (pitch-class r)
         mc (mode-class mc)
         pcs (pitch-classes-calc r (:degrees mc))]
    
    (->Mode n r mc pcs)))

