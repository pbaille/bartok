(ns utils.prob
  (:use [utils.utils]))

;take a map of object/prob pairs
(defn weight-picker [m]
  (let [sums (cons 0 (map-reduce + 0 (vals m)))
        parts (map #(hash-map :obj %1 :min (first %2) :max (second %2)) 
                    (keys m) 
                    (partition 2 1 sums))]
    (defn f 
      ([]
        (let [x (rand (last sums))
              l (select-first #(<= (:min %) x (:max %)) parts)]
          (:obj l)))
      ([x] (take x (repeatedly f))))))

;take a map of object/prob pairs
(defn weight-pick-one [m] ((weight-picker m)))