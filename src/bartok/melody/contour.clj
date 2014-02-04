(ns bartok.melody.contour
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.melody.melodic-domain))

(defn contour-transform
  "given bounds at start [dwn-bound-dist up-bound-dist]
  and contour-analysis seq [[n-successive-same-dir-steps sum] ... ]
  returns all possible transformations
  
  ex: 
  (contour-transform [7 7] [[3 -4] [2 4][3 -6]])
  => (([3 -3] [2 2] [3 -5]) ([3 -3] [2 3] [3 -6]) ... )
  
  (contour-transform [7 7] [[3 4][2 -4][3 6][2 -7]])
  => (([3 3] [2 -2] [3 3] [2 -5]) ([3 3] [2 -2] [3 4] [2 -6])  ... ) 
  "
  [[bd bu] cntr-an]
  (let [sizes (map first cntr-an)
        syms (mapv #(gensym (str "x" % "-"))(range (count cntr-an)))
        sum (apply + (map second cntr-an))
        [cntr-an-bd cntr-an-bu] (steps-bounds (map second cntr-an))
        fit-in-bounds? (and (<= (abs cntr-an-bd) bd) 
                            (<= cntr-an-bu bu))]
    (cond 
     ;if only one segment no transformation possible, returns wrapped cntr-an
     (count= cntr-an 1) [cntr-an] 
     fit-in-bounds?
     (eval
       `(for [~@(mapcat 
                  (fn [sym [n s] idx] 
                    [sym 
                     (cond 
                       (pos? s) `(range ~n (inc (- ~bu ~@(take idx syms))))
                       (neg? s) `(range (+ ~@(take idx syms) (- ~bd)) (inc (- ~n)))
                       (zero? s) [0])]) 
                  (butlast syms) 
                  cntr-an 
                  (range)) 
              :let [~(last syms) (- ~sum (a + ~(vec (butlast syms))))]
              :when (and (>= (abs ~(last syms)) ~(last sizes))
                         (same-sign? ~(last syms) ~(second (last cntr-an))))]
          (map vector ~(vec sizes) ~(vec syms))))
     
     :else ;if doesn't fit in bounds return nil
     (pp "*contour-transform*" "cntr-an:"cntr-an 
         "doesn't fit in bounds:" [bd bu] ))))

; little function to test bounds of output
;((juxt (p a min) (p a max)) (flatten (map #(steps-bounds (map second %)) *1)))

(defn- split-prob-map-by-key-sign [pm]
  (map tups->h-map ((juxt filter remove) #(neg? (key %1)) pm)))

(defn contour-analysis 
  "given a step sequence returns a seq of 
  [n-successive-same-dir-steps sum-of-steps]
  ex:
  (contour-analysis [1 2 3 -1 -2 3])
  =>([3 6] [2 -3] [1 3])"
  [steps]
  (->> (partition-by (p compare 0) steps)
       (map (juxt count #(a + %)))))

(defn- contour-segment->steps 
  "takes a contour segment 
  [n-successive-same-dir-steps sum-of-steps]
  return an equivalent step seq (integer vector)"
  [posints negints [siz sum :as cntrseg]]
  (let [ints-pm (if (pos? sum) posints negints)
        combs (dom-part (keys ints-pm) siz sum)]
    (when (seq combs)
      (-> (sort-by #(a + (map (p get ints-pm) %)) > combs)
          first 
          shuffle))))

(defn- apply-steps [negints posints cntr-an]
  (map (p contour-segment->steps posints negints) cntr-an))

(defn- part-transform
  [negz poz {[bd bu :as bnds] :bounds :as acc} zone]
  (let [f-res (select-first 
                (fn [x] (not (or-dr (in? x nil)))) 
                (map (p apply-steps negz poz) 
                     (shuffle (contour-transform bnds zone))))
        res-sum (a + (flatten f-res))
        n-bnds [(+ bd res-sum)(- bu res-sum)]]
    (-> acc 
        (update-in [:results] conj f-res)
        (assoc :bounds n-bnds))))

(defn contour-prob-line
  "args 
  - step sequence (3 -2 1 -4 -1 2 ...)
  - int prob map {1 0.3 -1 0.5 ...}
  - melodic-domain
  - transformation depth 
  compute a possible sequence of steps in respect of all args"
  [steps pm md depth]
  (if (step-sequence md steps) ;if steps fit in domain
    (let [md-bounds (interval-bounds md)
         [negints posints] (split-prob-map-by-key-sign pm)]
      (->> (partition depth depth nil (contour-analysis steps))
        (reduce (p part-transform negints posints)
               {:bounds (map (c abs to-num) (interval-bounds md)) 
                :results []})
        :results flatten))
    (prn "this contour doesn't fit in this domain (src/examples/xml_parse_tweak.clj line: 56)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




