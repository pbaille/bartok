(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def score (parse-xml "music-files/xml/jedall.xml"))

(def d-ints (voice->d-int (take 200 (second (process-score score)))))
(def contour (voice->contour (take 200 (second (process-score score)))))
(def clyd (melodic-domain :C-Lyd [:C-2 :C3] :C0))

(grid-assoc :tempo 120)

(defn rav-steps 
  "map 'jeux d'eau' steps on alternate C-Lyd and Ab-Lyd modes"
  [md len]
  (as-> (take len d-ints) x
        (map (fn [steps mod] 
              {:mode mod :steps steps}) 
             (partition 16 16 x) (cycle [:C-Lyd :Ab-Lyd]))
        (step-sequence x [:C-4 :C4] :C0)
        ; (dr)
        (m-note-line-from (g-pos 0 0 0) 1/4 60 1 x)))

;constrained markov generator with accumulator
(def cmgwa (->> (take 150 d-ints)
                (markov-depth-analysis 3 4)
                c-markov-gen-with-acc))

(defn rav-step-line 
  "return a m-note sequence based on 'jeux d'eau' top voice steps"
  [len]
  (->> (take len (cmgwa 
                  ;constraints
                  (fn [mel-dom chain-so-far stp]
                    (and 
                      ;check if stp is a possible step on mel-dom
                      (step mel-dom stp)
                      ;remove repetitions (step 0)
                      (not= 0 (to-num stp))))
                  ;init mel-domain
                  clyd
                  ;called to update domain at each step
                  (fn [acc nxt] (step acc nxt))))
       ;construct a step sequence based on returned chain
       (step-sequence clyd)
       ;to midi-notes
       (m-note-line-from (g-pos 0 0 0) 1/4 60 1)))

;------------------------------
;;; contour-prob-line work ;;;;
;------------------------------

(defn map-nth [n f coll]
  (mapcat #(ap vector (f (first %)) (next %)) 
          (partition n n nil coll)))

(defn- neg2nds [coll start]
  (if (= start 0)
    (map-nth 2 - coll)
    (cons (first coll) (map-nth 2 - (next coll)))))

(defn- weird+ [coll start]
  (a + (neg2nds coll start)))

(defn- same-sign? [x y] (pos? (* x y)))

;turn this into regular function
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
        ;if contour starts with ascendant segment pola is 1, else 0
        pola (if (< 0 (second (first cntr-an))) 1 0)
        [cntr-an-bd cntr-an-bu] (steps-bounds (map second cntr-an))
        fit-in-bounds? (and (<= (abs cntr-an-bd) bd) 
                            (<= cntr-an-bu bu))]
    (cond 
     (count= cntr-an 1) [cntr-an] 
     fit-in-bounds?
     (eval
       `(for [~@(mapcat 
                  (fn [sym cntrseg idx] 
                    [sym `(range ~(first cntrseg) 
                                  (inc (+ ~(if (= 0 pola) 
                                             (if (even? (second cntrseg)) bd bu)
                                             (if (even? (second cntrseg)) bu bd )) 
                                          ~@(take idx syms))))]) 
                  (butlast syms) 
                  cntr-an 
                  (range)) 
              :let [~(last syms) (- ~sum (weird+ ~(vec (butlast syms)) ~pola))]
              :when (and (>= (abs ~(last syms)) ~(last sizes))
                         (same-sign? ~(last syms) ~(second (last cntr-an))))]
          (map vector ~(vec sizes) (conj (vec (neg2nds (butlast ~syms) ~pola)) 
                                         ~(last syms)))))
     
     :else
     (pp "*contour-transform*" "cntr-an:"cntr-an 
         "doesn't fit in bounds:" [bd bu] ))))

;model for (= 3 (count cntr-an))
  ; (defn contour-transform-3 [[bd bu][[n1 s1][n2 s2][n3 s3]]]
  ;   (let [sum (+ s1 s2 s3)]
  ;     (for [a (range n1 (inc bu))
  ;           b (range n2 (inc (+ a bd)))
  ;           :let [c (- sum (weird+ [a b]))]
  ;           :when (>= c n3)]
  ;       [a b c])))

(defn- split-prob-map-by-key-sign [pm]
  (map tups->h-map ((juxt filter remove) #(neg? (key %1)) pm)))

(defn- contour-analysis 
  "given a step sequence returns a seq of 
  [n-successive-same-dir-steps sum-of-steps]
  ex:
  (contour-analysis [1 2 3 -1 -2 3])
  =>([3 6] [2 -3] [1 3])"
  [steps]
  (->> (partition-by neg? steps)
       (map (juxt count #(a + %)))))

(defn- cntrseg->d-ints [posints negints [siz sum :as cntrseg]]
  (let [ints-pm (if (pos? sum) posints negints)
        ; _ (dr)
        combs (dom-part (keys ints-pm) siz sum)]
    
    (when (seq combs)
      (-> (sort-by #(a + (map (p get ints-pm) %)) > combs)
          first 
          shuffle))))

(defn apply-steps [negints posints cntr-an]
  ; (dr)
  (map (p cntrseg->d-ints posints negints) cntr-an))

(defn- zone-helper 
  [{[bd bu :as bnds] :bounds poz :posints negz :negints :as acc} zone]
  (let [f-res (select-first 
              (fn [x] (not (try-dr (in? x nil)))) 
              (map (p apply-steps negz poz) 
                   (shuffle (contour-transform bnds zone))))
        ; _ (dr)
        res-sum (a + (flatten f-res))
        n-bnds [(+ bd res-sum)(- bu res-sum)]]
    ; (pp "***" "zone" zone "f-res" f-res "n-bnds" n-bnds)
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
     (reduce #(zone-helper %1 %2)
             {:bounds (map (c abs to-num) (interval-bounds md)) 
              :posints posints 
              :negints negints 
              :results []} 
             (partition depth depth nil (contour-analysis steps))))
    (prn "this contour doesn't fit in this domain (src/examples/xml_parse_tweak.clj line: 56)")))

;; test ;;;

; (def ints-vals (map to-num d-ints))
; (def fpm (zipmap ints-vals (repeat 1)))
; (def sfpm (split-prob-map-by-key-sign fpm))

; (def transcntr (flatten (:results (contour-prob-line (take 50 ints-vals) fpm clyd 3))))
; (steps-bounds transcntr)
; (map :val (interval-bounds (step clyd 8)))
; (def ss (step-sequence (step clyd 8) transcntr))
; (def mn-line (m-note-line-from (g-pos 0 0 0) 1/4 60 1 ss))

; (grid {:bars [[24 :4|4]] :tempo 120})
; (play @*midi-out* mn-line)


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defn- contour-partial-transformation [bnds partial-cntr]
;   ((functionize contour-transform) bnds partial-cntr))

; (defn contour-variations [contour-an md depth]
;   (let [md-bounds (interval-bounds md)]
;      (reduce #(let [[bd bu :as bnds] (:bounds %1)
;                     part (first (shuffle (contour-partial-transformation bnds %2)))
;                     sum (a + (map second part))]
;                 (-> (update-in %1 [:results] into part)
;                     (assoc :bounds [(+ bd sum)(- bu sum)])))
;              {:bounds (map (c abs to-num) md-bounds) 
;               :results []} 
;              (partition depth contour-an))))

; (contour-variations 
;   [[3 9] [3 -10] [3 9] [1 -8] [3 9] [3 -10] [3 9] [2 -14] [1 1]]
;   clyd
;   3)

; (defn- contour-segment->steps 
;   [posints negints [siz sum :as cntrseg]]
;   (let [ints-pm (if (pos? sum) posints negints)
;         combs (dom-part (keys ints-pm) siz sum)]
;     (when (seq combs)
;       (-> (sort-by #(a + (map (p get ints-pm) %)) > combs)
;           first 
;           shuffle))))


