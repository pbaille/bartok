(ns bartok.melody.step-pattern
  (:use [utils.dom-part])
  (:use [bartok.melody.melodic-domain])
  (:use [clojure.math.combinatorics :as c])
  (:use [utils.utils]))

;************* helpers ********************

(def ^:private default-params
  {:steps #{-3 -2 2 3}
   :iterations #{4}
   :cycle-steps #{-3 -2 2 3}
   :cycle-lengths #{3 4 5}})

(defn- keys-subset? [sub m]
  (clojure.set/subset? (set (keys sub)) (set (keys m))))

(defn- merge-with-defaults [m]
  (when (keys-subset? m default-params)
        (merge default-params m)))

(defn- amplitude [step-sequence]
  (reduce (fn [{:keys [up down total-step]} el]
            (let [acc (+ total-step el)
                  up (if (> acc up) acc up)
                  down (if (< acc down) acc down)]
              {:up up :down down :total-step acc}))
          {:down 0 :up 0 :total-step 0}
          step-sequence))
      
; (defn- steps-calc-old [params]
;   (mapcat (fn [{:keys [cycle-length cycle-step] :as m}] 
;             (map #(merge m (hash-map :step-pattern %)) 
;                   (dom-part (:steps params) cycle-length cycle-step)))
;           (for [cs (:cycle-steps params) 
;                 cl (:cycle-lengths params)]
;             {:cycle-length cl :cycle-step cs})))


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
  (let [mps (step-patterns params)
        emps (shuffle (mapcat expand-step-pattern mps))
        cnt  (count emps)]
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
 
 