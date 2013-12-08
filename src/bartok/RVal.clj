(ns bartok.RVal
  (:use [utils.utils])
  (:use [clojure.math.combinatorics :as c]))

(defn is-ratio? [r] (= (type r) clojure.lang.Ratio))

(defn denominator [r] 
  (if (is-ratio? r) (clojure.core/denominator r) 1))

(defn to-ms [rational bpm]
  (* (/ 60000 bpm) rational))

(defn allowed-subs [rval]
  (set 
    (for [sub (-> rval denominator prime-factors c/subsets)]
      (apply * sub))))

(defn rand-rval-at [rvals at]
  (let [allowed (filter #(in? (allowed-subs %) (denominator at)) 
                        (seq rvals))]
    (rand-nth allowed)))

(defn r-line
  ([rvals] (r-line rvals 0))
  ([rvals at]
   (lazy-seq
     (let [v (rand-rval-at rvals at)]
       (cons v (r-line rvals (+ v at)))))))


