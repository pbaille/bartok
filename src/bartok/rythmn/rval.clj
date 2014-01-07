(ns bartok.rythmn.rval
  (:use [utils.utils])
  (:use [clojure.math.combinatorics :as c]))

(defn denom [r] 
  (if (ratio? r) (denominator r) 1))

(defn to-ms [rational bpm]
  (* (/ 60000 bpm) rational))

(defn allowed-subs [rval]
  (set (for [sub (-> rval denom prime-factors c/subsets)]
         (apply * sub))))





