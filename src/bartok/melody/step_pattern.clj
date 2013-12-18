(ns bartok.melody.step-pattern
  (:use [utils.dom-part])
  (:use [utils.utils]))

;************* helpers ********************

(def ^:private default-params
  {:steps (set (range -2 3))
   :iterations #{2 3 4}
   :cycle-steps (set (range -3 4))
   :cycle-lengths #{3 4}})

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
      
(defn- steps-calc [params]
  (mapcat (fn [{:keys [cycle-length cycle-step] :as m}] 
            (map #(merge m (hash-map :step-pattern %)) 
                  (dom-part (:steps params) cycle-length cycle-step)))
          (for [cs (:cycle-steps params) 
                cl (:cycle-lengths params)]
            {:cycle-length cl :cycle-step cs})))

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

(defn step-patterns [params] 
  (let [params (merge-with-defaults params)] 
    (map #(add-iterations % (:iterations params)) 
         (for [sp (steps-calc params)] sp ))))

(defn step-pattern-picker [params]
  (let [mps (step-patterns params)
        emps (shuffle (mapcat expand-step-pattern mps))
        cnt  (count emps)]
    (fn [[down up]]
      (select-first #(and (>= (get-in % [:amplitude :down]) down)
                          (<= (get-in % [:amplitude :up]) up))
                    (rotate emps (rand-int cnt)) ))))

 
 