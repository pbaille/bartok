(ns bartok.melody.step-pattern
  (:use [utils.dom-part])
  (:use [bartok.melody.melodic-domain])
  (:use [clojure.math.combinatorics :as c])
  (:use utils.profile)
  (:use [utils.utils]))

;************* helpers ********************

(def ^:private default-params
  {:steps #{-4 -3 -2 2 3 4}
   :iterations #{3 4 5 6}
   :cycle-steps #{-4 -3 -2 2 3 4}
   :cycle-lengths #{3 4 5 6}})

(defn- keys-subset? [sub m]
  (clojure.set/subset? (set (keys sub)) (set (keys m))))

(defn- merge-with-defaults [m]
  (when (keys-subset? m default-params)
        (merge default-params m)))

(defn amplitude [step-sequence]
  (reduce (fn [{:keys [up down total-step]} el]
            (let [acc (+ total-step el)
                  up (if (> acc up) acc up)
                  down (if (< acc down) acc down)]
              {:up up :down down :total-step acc}))
          {:down 0 :up 0 :total-step 0}
          step-sequence))

(defn- steps-calc [params]
  (apply concat 
    (for [cs (:cycle-steps params) 
          cl (:cycle-lengths params)]
      (map #(assoc {:cycle-length cl :cycle-step cs} :step-pattern %) 
           ; heavy ... maybe should compute permutations later in step-pattern-picker !!!!!!!!!!!
           (mapcat c/permutations (dom-part (:steps params) cl cs))))))

(defn- add-iterations [step-pattern iterations]
  (let [iters 
         (for [i iterations] 
           (let [sequence (->> step-pattern :step-pattern (repeat i) flatten)
                 {:keys [up down total-step]} (amplitude sequence)]
             {i {:sequence sequence 
                 :amplitude {:up up :down down} 
                 :total-step total-step}}))]
    (merge step-pattern {:iterations (apply merge iters)})))

(defn- expand-step-pattern [mp]
  (for [i (:iterations mp)]
    (let [mp (dissoc mp :iterations)]
      (conj mp (conj {:iterations (first i)} (second i))))))

;**************** public *********************

(defn step-patterns  
  ([] (step-patterns {}))
  ([params]
    (let [params (merge-with-defaults params)] 
      (for [sp (steps-calc params)] 
        (add-iterations sp (:iterations params))))))

(defn step-pattern-picker [params]
  (let [mps (prof :mps (step-patterns params))
        emps (prof :emps (mapcat expand-step-pattern mps))
        cnt  (prof :count (count emps))]
    (fn fun
      ([md] (apply fun (map :val (interval-bounds md))))
      ([down up] 
       (select-first #(and (>= (-> % :amplitude :down) down)
                           (<= (-> % :amplitude :up) up)) 
                      (rotate emps (rand-int cnt)))))))

(defn step-patterns-line [md picker]
  (lazy-seq 
    (let [pat (vec (step-sequence md (:sequence (picker md))))
          md (set-current md (last pat))]
      (concat pat (step-patterns-line md picker)))))

(defn steps-line [bounds picker]
  (lazy-seq 
    (let [pat (apply picker bounds)
          bounds (map #(- % (:total-step pat)) bounds)]
      (concat (:sequence pat) (steps-line bounds picker)))))

;********************* NEW ********************

(defn- steps-calc-new [params]
  (apply concat 
    (for [cs (shuffle (:cycle-steps params)) 
          cl (shuffle (:cycle-lengths params))]
      (map #(assoc {:cycle-length cl :cycle-step cs} :step-pattern %) 
           (dom-part (:steps params) cl cs)))))

(defn sp-permutations [sp]
  (map #(assoc sp :step-pattern %)
       (c/permutations (:step-pattern sp))))

(defn expand-iterations [step-seq n]
  (apply concat (repeat n step-seq)))

(defn step-patterns-new  
  ([] (step-patterns-new {}))
  ([params] (steps-calc-new (merge-with-defaults params))))

; apparently slower than regular step-pattern-picker on little sets
; but way way faster on larger
(defn lazy-step-pattern-picker [params]
  (let [params (merge-with-defaults params)]
    (fn fun
      ([md] (apply fun (map :val (interval-bounds md))))
      ([down up] 
       (prof :main (let [mps (step-patterns-new params)
             ff (prof :ff (fn [dwn u x]
                  (first-truthy 
                    (fn [[per i]]
                      (let [s (expand-iterations (:step-pattern per) i)
                            {:keys [down up total-step]} (amplitude s)]
                        (when (and (>= down dwn) (<= up u))
                          (assoc per :sequence s :amplitude {:down down :up up} :total-step total-step)))) 
                    (for [i (shuffle (:iterations params))
                          :when (let [step (* i (:cycle-step x))]
                                  (cond
                                    (and (<= 0 step) (> step up)) false
                                    (and (> 0 step) (< step down)) false
                                    :else true))
                          ; this shuffle is bad for perf but good for randomness of the picker
                          per (shuffle (sp-permutations x))]
                      [per i]))))] 
         (first-truthy (partial ff down up) mps))))))) 

;similar performance ...
; (defn lazy-step-pattern-picker2 [params]
;   (let [params (merge-with-defaults params)]
;     (fn fun
;       ([md] (apply fun (map :val (interval-bounds md))))
;       ([down up] 
;        (prof :main(let [mps (step-patterns-new params)
;              ff (fn [dwn u x]
;                   (first-truthy 
;                     (fn [[per i]]
;                       (let [s (expand-iterations (:step-pattern per) i)
;                             {:keys [down up total-step]} (amplitude s)]
;                         (when (and (>= down dwn) (<= up u))
;                           (assoc per :sequence s :amplitude {:down down :up up} :total-step total-step)))) 
;                     (let [iters (filter  #(let [step (* % (:cycle-step x))] 
;                                             (cond
;                                               (and (<= 0 step) (> step up)) false
;                                               (and (> 0 step) (< step down)) false
;                                               :else true)) 
;                                         (:iterations params))]
;                       (for [i iters per (sp-permutations x)]
;                         [per i]))))] 
;          (first-truthy (partial ff down up) mps))))))) 
 