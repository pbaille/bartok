(ns bartok.rythmn.rval
  (:use [utils.utils])
  (:use [bartok.structure.grid])
  (:use [clojure.math.combinatorics :as c]))

(defn denom [r] 
  (if (ratio? r) (denominator r) 1))

(defn to-ms [rational bpm]
  (* (/ 60000 bpm) rational))

(defn allowed-subs [rval]
  (set (for [sub (-> rval denom prime-factors c/subsets)]
         (apply * sub))))

; (defn allowed-subs [rval]
;   (set 
;     (for [sub (-> rval denom prime-factors c/subsets)]
;       (apply * sub))))

; (defn rand-rval-at [rvals at]
;   (let [allowed (filter #(in? (allowed-subs %) (denom at)) 
;                         (seq rvals))]
;     (rand-nth allowed)))

; (defn r-line
;   ([rvals] (r-line rvals 0))
;   ([rvals at]
;    (lazy-seq
;      (let [v (rand-rval-at rvals at)]
;        (cons v (r-line rvals (+ v at)))))))





