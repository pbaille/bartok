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

;return a drunk function
(defn drunk-sequencer 
  ([range-bounds max-step] 
    (drunk-sequencer range-bounds max-step (int (a median range-bounds)) false))
  ([range-bounds max-step start] 
    (drunk-sequencer range-bounds max-step start false))
  ([range-bounds max-step start rep-bool]
  (let [r (- (second range-bounds) (first range-bounds))
        steps (if rep-bool 
                (range (- max-step) (inc max-step))
                (concat (range (- max-step ) 0)
                        (range 1 (inc max-step))))]
    (fn fun 
      ([] (fun start))
      ([start]
      (let [available-steps (filter #(between (+ start %) range-bounds) steps)
            next (+ start (rand-nth available-steps))]
        (lazy-seq (concat [start] (fun next)))))))))