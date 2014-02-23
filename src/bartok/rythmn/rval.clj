(ns bartok.rythmn.rval
  (:use utils.utils)
  (:use [clojure.math.combinatorics :as c]))

(defn denom [r] 
  (if (ratio? r) (denominator r) 1))

(defn to-ms [rational bpm]
  (* (/ 60000 bpm) rational))

(defn allowed-subs [rval]
  (set (for [sub (-> rval denom prime-factors c/subsets)]
         (apply * sub))))

(def bin-vals 
  [1 2 4 8 16 32 64])

(defn bin-rval? [rval]
  (in? bin-vals (denom rval)))

(defn bin-resolution [rval]
  (* rval (poly-base rval)))

(defn poly-base [rval]
  (iterate-while integer? #(/ % 2) (denom rval)))




