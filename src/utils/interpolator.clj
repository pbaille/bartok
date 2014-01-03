(ns utils.interpolator
  (:use utils.utils))

(defn- expand-triplets [points]
  (reduce 
    #(if (count= %2 3) 
      (merge %1 
         [(first %2) (nth %2 2)] 
         [(second %2) (nth %2 2)])
      (merge %1 %2)) 
    (sorted-set) 
    points))

;(expand-triplets [[1 4 20][8 34]]) 
; => #{[1 20] [4 20] [8 34]}
; allow to pass easily constant val range into interpolator

(defn interpolator [points]
  (let [m (expand-triplets points)]
    (fn fun 
      ([x]
      (let [[[x1 y1]] (reverse (filter #(<= (first %) x) m))
            [[x2 y2]] (filter #(> (first %) x) m)
            [[x1 y1] [x2 y2]]
              ; if x is out of bounds
              (cond
                (and x1 x2) [[x1 y1] [x2 y2]]
                x1 (reverse m)
                x2 (vec m))]
        (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1))))))
      ; median value of a range
      ([xa xb]
        (let [between-points (filter #(and (> (first %) xa) (< (first %) xb)) m)]
          (if (seq between-points)
            (let [weights (concat [(- (-> between-points first first) xa)]
                                  (steps (map first between-points))
                                  [(- xb (-> between-points last first))])
                  xs (concat [xa] (map first between-points) [xb])
                  meds (map #(apply fun %) (partition 2 1 xs))
                  w-meds (map * meds weights)]
              (/ (reduce + w-meds) (reduce + weights)))
            (fun (median xa xb))))))))

(defn cyclic-interpolator [points len]
  (let [points 
        (concat points 
                (map (fn [[a b c]] (if c [(+ a len)(+ b len) c] [(+ a len) b])) 
                     points))
        f (interpolator points)]
    (fn fun 
      ([x] (f (mod x len)))
      ([xa xb] 
       (let [xa (mod xa len)
             xb (mod xb len)
             xb (if (> xa xb) (+ xb len) xb)]
         (f xa xb))))))

;; => (map (interpolator [[0 0] [1 1] [3 2] [4 3]]) (range 0 9/2 1/2))
;; (0 1/2 1 5/4 3/2 7/4 2 5/2 3)
