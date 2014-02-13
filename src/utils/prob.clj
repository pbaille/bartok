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

(defmacro prob-exprs 
  "randomly choose an expression accordingly to all weights and eval it
  (prob-exprs 1 (+ 2 3) 3 (name :yo))
  1/4 chances to => 5 
  3/4 chances to => 'yo' "
  [& body]
  (let [exprs (take-nth 2 (next body))
        weights (take-nth 2 body)
        siz (/ (count body) 2)]
   `(case (weight-pick-one ~(zipmap (range siz) weights)) 
      ~@(interleave (range siz) exprs))))

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

;-------------------------------------------
;;;;;;;;;;;;;;;;; markov ;;;;;;;;;;;;;;;;;;;
;-------------------------------------------

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

(defn get-probs [s data]
  (->> data
    (filter #(loop [q s] 
               (cond (= (first %) q) true 
                     (seq q) (recur (next q))  
                     :else false)))
    (a concat)
    (a hash-map)
     vals
    (a merge-with +)))

(->> [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62]
     (markov-depth-analysis  3 [0.1 2 3])
     (get-probs [60 64 67]))

(defn markov-gen 
  "
  return a markov-chain generator that can be call with any number of arguments
  with no argument it return a lazy markov-chain that start on random val
  with 1 arguments or more it return the lazy markov-chain that starts with args
  
    (def laz (->> [60 62 64 66 67 69 71 69 67 66 64 62 60 64 67 71 69 66 62]
       (markov-depth-analysis  3 [0.1 2 3])
       markov-gen))

    (take 10 (laz))
    (take 10 (laz 66))
    (take 10 (laz 66 67))
  "
  [data]
  (let [depth (or (:depth data) 
                  (a max (map count (keys data))))
        fun (fn fun 
              [chain probs] 
              (let [v (vec (vals probs))
                    nxt (nth (keys probs) (wrand v))
                    nchain (conj chain nxt)]
                (lazy-seq
                  (cons nxt (fun nchain (get-probs (take-last depth nchain) data))))))]
    (fn ([] (fun [] (get data (rand-nth (keys data)))))
        ([x] (cons x (fun [] (get data [x]))))
        ([x & xs] 
          (let [els (into [x] xs)]
            (lazy-cat els (fun els (get data els))))))))

(defn c-markov-gen 
  "same as markov-gen but returned fn take a first extra argument:
   'constrfn' that is a predicate called on : [chain-so-far prob-key] 
   in order to remove elements that doesn't satisfies it
  
  examples: 
  
    (def cmg (->> (repeatedly 50 (p rand-int-between -3 3))
                  (markov-depth-analysis  3 [0.1 2 3])
                  c-markov-gen))
    ;with chain
    (take 10 (cmg (fn [chain el] 
                        (not= (abs el) (abs (last chain)))) 
                      [-3 2]))
    ;with start
    (take 10 (cmg (fn [chain el] 
                        (not= (abs el) (abs (last chain)))) 
                      -3))
    ;with random start
    (take 10 (cmg (fn [chain el] 
                    (not= (abs el) (abs (last chain))))))
  "
  [data]
  (let [depth (or (:depth data) 
                  (a max (map count (keys data))))
        fun (fn fun 
              [chain probs constraints-fn] 
              (let [v (vec (vals probs))
                    nxt (nth (keys probs) (wrand v))
                    nchain (conj chain nxt)
                    possibilities (filter #(constraints-fn nchain (key %1)) 
                                      (get-probs (take-last depth nchain) data))]
                (when (seq possibilities)
                  (lazy-seq
                    (cons nxt (fun nchain 
                                   possibilities
                                   constraints-fn))))))]
    (fn ([constr] (fun [] (get data (rand-nth (keys data))) constr))
        ([constr chain-or-start] 
          (if (vector? chain-or-start) 
            (lazy-cat chain-or-start (fun chain-or-start (get data chain-or-start) constr))
            (cons chain-or-start (fun [] (get data [chain-or-start]) constr)))))))

(defn c-markov-gen-with-acc 
  "
  same as markov-gen but returned fn takes 3 more args:
  'constrfn' that is a predicate called on : [accumulator chain-so-far prob-key]
   in order to remove elements that doesn't satisfies it
  acc is the initial state of the accumulator
  acc-upd is the fun called at each step on [acc (last chain-so-far)] to update acc
  
  ex: construct a step sequence based on markov analysis of another step sequence
      at each step domain need to be check in order to avoid out of bounds steps 
  
    (def c-laz (->> (repeatedly 50 (p rand-int-between -3 3))
                    (markov-depth-analysis  3 [0.1 2 3])
                    c-markov-gen-with-acc))

    (use 'bartok.melody.melodic-domain)
    (use 'bartok.primitives)

    (->> (take 10 (c-laz 
                    ;check if stp is a possible step on mel-dom
                    (fn [mel-dom chain-so-far stp]
                      (step mel-dom stp))
                    ;init mel-domain
                    (melodic-domain :C-Lyd [:C-1 :C2] :C0)
                    ;called to update domain at each step
                    (fn [acc nxt] (step acc nxt))))
         ;construct a step sequence based on returned chain
         (step-sequence (melodic-domain :C-Lyd [:C-1 :C2] :C0))
         ;map it to name for readability
         (map :name))
  "
  [data]
  (let [depth (or (:depth data) 
                  (a max (map count (keys data))))
        fun (fn fun 
              [chain probs constraints-fn acc acc-update] 
              (let [v (vec (vals probs))
                    nxt (nth (keys probs) (wrand v))
                    nchain (conj chain nxt)
                    nacc (acc-update acc nxt)
                    possibilities (filter #(constraints-fn nacc nchain (key %1)) 
                                      (get-probs (take-last depth nchain) data))]
                (if (seq possibilities)
                  (lazy-seq
                    (cons nxt (fun nchain 
                                   possibilities
                                   constraints-fn
                                   (when nacc nacc)
                                   (when nacc acc-update))))
                  nil)))]
    (fn ([constrfn acc acc-upd] 
          (fun [] 
               (get data (rand-nth (keys data)))
                constrfn 
                acc
                acc-upd))
        ([constrfn acc acc-upd start-or-chain] 
          (let [start-or-chain (vec-if-not start-or-chain)]
            (when-let [gdat (get data start-or-chain)]
              (lazy-cat start-or-chain
                (fun start-or-chain 
                  gdat
                  constrfn 
                  acc
                  acc-upd))))))))





