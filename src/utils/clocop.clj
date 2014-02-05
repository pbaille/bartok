(ns utils.clocop
  (:use [clocop core constraints])
  (:use utils.all)
  (:use [clojure.template]))

(defn gensyms 
  "generate a list of syms
  ex: (gensyms 5) 
  => [a2572 b2573 c2574 d2575 e2576]"
  [n]
  (mapv #(-> % char str symbol gensym) (range 97 (+ 97 n))))

(defmacro $>all [item coll]
  `($and ~@(map (fn [i] `($> ~item ~i)) coll)))

(defmacro $<all [item coll]
  `($and ~@(map (fn [i] `($< ~item ~i)) coll)))

(defmacro $map-and [constraint item coll]
  `($and ~@(map (fn [i] `(~constraint ~item ~i)) coll)))

(defmacro $map-or [constraint item coll]
  `($or ~@(map (fn [i] `(~constraint ~item ~i)) coll)))

(defmacro fn-template [f argv expr & values]
  `(~f ~@(map (fn [a] (apply-template argv expr a)) 
              (partition (count argv) values))))

(defmacro $mapc 
  "ex: ($mapc $and ($> _ 2) [x y z])
   _ is the placeholder for template
   => ($and ($> x 2) ($> y 2) ($> z 2))"
  [fn expr coll]
  `(fn-template ~fn [~'_] ~expr ~@coll))

(defmacro $mor [constraint item coll]
  `($mapc $or (~constraint ~item ~'_) ~coll))

(defn $pos [x]
  ($< 0 x))

(defn $neg [x]
  ($> 0 x))

(defn $zero [x]
  ($= 0 x))

(defmacro $same-sign [x y]
  `($or ($pos ($* ~x ~y))
        ($and ($zero ~x)($= ~x ~y))))

(defmacro $sum-to [s & els]
  `($= ~s ($+ ~@els)))

(defmacro $between [x [d u]]
  `($and ($>= ~x ~d) ($<= ~x ~u)))

(defmacro do-constrain! [& conss]
  `(do ~@(map (fn [x] `(constrain! ~x)) conss)))