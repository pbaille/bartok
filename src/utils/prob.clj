(ns utils.prob
  (:use [utils utils macros])
  (:use [vendors.debug-repl])
  (:require [clojure.contrib.math :as math]))

;take a map of object/prob pairs
(defn weight-picker [m]
  (let [sums (reductions + 0 (vals m))
        parts (map #(hash-map :obj %1 :min (first %2) :max (second %2)) 
                    (keys m) 
                    (partition 2 1 sums))]
    (fn f 
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

;**************** markov *******************

(defn wrand
  "given a vector of slice sizes, returns the index of a slice given a                                                                                                                                      
  random spin of a roulette wheel with compartments proportional to                                                                                                                                         
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn markov-analysis
  [mel]
  (->> mel
       (partition 2 1)
       (reduce (fn [acc [w next-w]]
                 (update-in acc
                            [w next-w]
                            (fnil inc 0)))
               {})))

(defn markov-chain-simple [data start len]
  (loop [ws (data start)
         acc []]
    (let [v (vec (vals ws))
          i (wrand v)
          n (nth (keys ws) i)]
      (if (= (count acc) len)
        acc
        (recur (data n) (conj acc n))))))

(defn markov-analysis-n
  ([len data]
  (->> data
       (partition (inc len) 1)
       (reduce (fn [acc el]
                 (update-in acc
                            [(vec (butlast el)) (last el)]
                            (fnil inc 0)))
               {}))))

;(markov-analysis-n 6 (take 100 (repeatedly #(rand-int 3))))

(defn markov-depth-analysis [depth factors data]
  (let [factors 
        (if (vector? factors) 
          factors 
          (iterate (p * factors) 1))]
    (->> (range depth)
         (map #(let [factor (nth factors %)
                     m (markov-analysis-n (inc %) data)]
                (reduce 
                  (fn [acc [k v]] 
                    (assoc acc k (map-vals (p * factor) v))) 
                  {:depth depth} 
                  m)))
         (a merge))))

;(markov-depth-analysis  3 [0.1 2 3] [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62])
;(markov-depth-analysis  3 0.5 [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62])

(defn- get-probs [s data]
  (->> data
    (filter #(loop [q s] 
               (cond (= (first %) q) true 
                     (seq q) (recur (next q))  
                     :else false)))
    (a concat)
    (a hash-map)
     vals
    (a merge-with +)))

; (->> [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62]
;      (markov-depth-analysis  3 [0.1 2 3])
;      (get-probs [60 64 67]))


(defn markov-chain [len start data]
  (let [depth (or (:depth data) 
                  (a max (map count (keys data))))]
    (loop [ws (get data [start])
           acc []]
      (let [v (vec (vals ws))
            n (nth (keys ws) (wrand v))]
        (if (= (count acc) len)
          acc
          (recur (get-probs (take-last depth (conj acc n)) data) 
                 (conj acc n)))))))

(->> [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62]
     (markov-depth-analysis  3 [0.1 2 3])
     (markov-chain 10 60))

;************* data *******************

; ;ravel ma mere l'oye
; (def rmmlo 
;   {:1 [90 87 85 87 82 90 85 87 87 90 87 85 87 82 87 90 85 87 90 87 85 87 82 85 80 82 78 80 75 80 82 85 87 80 82 75 78 80 82 85 78 80 75 75 78 80 82 75 80 78 78 90 87 85 87 82 90 85 87 87 90 87 85 87 82 87 90 85 87 90 87 85 87 82 85 80 82 78 80 78 73 75 70 75 78 80 82], 
;    :2 [61 63 68 70 66 68 75 73 61 63 70 68 66 68 73 75 61 68 70 63 66 68 75 73 70 73 75 61 68 70 63 66 73 68 75 70 73 75 61 68 70 63 66 73 75 68 70 75 73 61 63 70 68 66 73 75 68 70 75 73 61 63 70 68 66 68 75 73 70 75 73 61 71 68 66 63 61 71 68 66 63 61 71 68 66 63 61 70 68 63 66 75 73 68 70 73 75 61 70 68 63 66 73 75 68 70 73 75 61 70 68 63 66 75 73 68 70 73 61 63 56 66 63 61]})

; (def mc
;   (->> (:1 rmmlo)
;        (markov-depth-analysis  4 [0.5 1 2 3])
;        (markov-chain 100 80)))



