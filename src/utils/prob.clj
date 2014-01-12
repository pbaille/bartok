(ns utils.prob
  (:use [utils.utils])
  (:use [vendors.debug-repl])
  (:use [utils.macros]))

;take a map of object/prob pairs
(defn weight-picker [m]
  (let [sums (reductions + 0 (vals m))
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

;exemple usage of the defnaults macro...

(defnaults drunk-sequencer2
  [range-bounds [0 1] 
   max-step      1/5 
   start         1/2 
   rep-bool      false 
   resolution    1/10   ]
  (let [steps (if rep-bool 
                (range-by (- max-step) max-step resolution)
                (concat (range-by (- max-step ) (- 0 resolution) resolution)
                        (range-by resolution max-step resolution)))]
    (fn fun 
      ([] (fun start))
      ([start]
      (let [available-steps (filter #(between (+ start %) range-bounds) steps)
            next (+ start (rand-nth available-steps))]
        (lazy-seq (concat [start] (fun next))))))))

