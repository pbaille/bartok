(ns bartok.rythmn.skull
  (:use [bartok.rythmn rval utils])
  (:use bartok.structure)
  (:use [clojure.math.combinatorics :as c])
  (:use [utils utils prob dom-part macros]))

;;;;;;;;;;;; r-skull-helpers ;;;;;;;;;;;;;;

;maybe should move this to utils
(defn- doto-until 
  "like (first (drop-while (c not pred) (iterate f init))) "
  [pred f init]
  (loop [x init] 
    (if (pred x) 
      x 
      (recur (f x)))))

;maybe should move this to utils
(defn- doto-while
  "like (last (take-while pred (iterate f init)))
  returns nil if (not (pred init))"
  [pred f init]
  (loop [x init fx (f x)]
    (if (pred fx)
      (recur fx (f fx))
      (if (pred x) x nil))))

(defn min-rbase-val 
  "for given r-base return the minimum value that is greater than min
  (min-rbase-val 1/4 3) => 1/3 "
  [min base] 
  (let [baz (/ base)]
    (cond 
      (< baz min) (doto-until (p <= min) (p * 2) baz)
      (> baz min) (doto-while (p <= min) #(/ % 2) baz)
      :else baz)))

; (defn- r-skull-p-zones 
;   [cmplx rb-pm]
;   (let [rbases (keys sorted-rb-pm)
;         rb-min-vals (min-rbases-vals cmplx rbases)
;         rb-bin-res (map bin-resolution rb-min-vals)]
;     (zipmap 
;       rbases 
;       (map #(hash-map :prob %1 ;(/ %1 %3) 
;                       :unit %2 
;                       :size %3) 
;            (vals rb-pm)
;            rb-min-vals 
;            rb-bin-res))))

(defn- poly-zones 
  "assign a map {:unit _ :size _ :prob _} to each rbases
  unit => resolution-unit of the rbase zone
  size => size of a zone 
  prob => prob of a zone to occur
  a zone is the minimum size that a rbase unit need to resolve on a binary subdivision"
  [cmplx rb-pm]
  (let [p-zones (map-h
                  (fn [base prob]
                   (let [unit (min-rbase-val cmplx base)
                         size (bin-resolution unit)]
                    {base {:prob prob :unit unit :size size}}))
                  rb-pm)]
    ; if base 2 is here make his size as large as the minimum size of others bases
    (if (p-zones 2) 
      (assoc-in p-zones [2 :size] 
        (second (sort (map :size (vals p-zones)))))
      p-zones)))

(defn- r-skull-len [skull]
  (a + (map :len skull)))

(defn- apply-ph-prob 
  "apply poly-homogeneity probability to work-map"
  [ph skull p-zones]
  (if-not (seq skull)
    p-zones
    (let [last-base (:base (last skull))
          last-base-val (p-zones last-base)
          other-bases (dissoc p-zones last-base)
          action (cond 
                    (< ph 0.5) :destabilize
                    (> ph 0.5) :stabilize
                    :else :neutral)
          factor (scale-range (abs (- ph 0.5)) 0.5 0 0 1)]
      (case action 
        :destabilize 
        (conj {last-base (update-in last-base-val [:prob] * factor)} 
              other-bases)
        :stabilize 
        (conj {last-base last-base-val} 
              (map-vals #(update-in % [:prob] * factor) other-bases))
        :neutral  
        p-zones))))

(defn- append-next 
  "add the last chosen zone to skull"
  [skull p-zones len]
  (let [skull-len (r-skull-len skull)
        current-sub (denom skull-len)
        remaining-len (- len skull-len)
        filtered 
        (filt-h 
          #(and 
             (>= remaining-len (:size (val %)))
             (in? (allowed-subs (:size (val %))) current-sub)) 
          p-zones)
        prob-map (map-vals :prob filtered)
        chosen (weight-pick-one prob-map)]
    (if (not= chosen (:base (last skull)))
      (conj skull {:base chosen 
                   :unit (:unit (p-zones chosen)) 
                   :len (:size (p-zones chosen))})
      (conj 
        (vec (butlast skull)) 
        (update-in (last skull) [:len] + (:size (p-zones chosen)))))))

;;;;;;;;;;;;;;;;;;; r-skull ;;;;;;;;;;;;;;;;;;;;;;

(defn r-skull 
  "
  len :: (Int) ;; length of the skull 
  (maybe allow ratio too, but more complex implementation) 
  options: (see poly-zones destructuring)
    - complexity :: (ratio) ;; minimum allowed subdivision
    - rb-pm :: ({(r-base :: (Num)) (prob :: (Num))}) ;; probability of each r-base to occur
    - ph :: (0 < (Float) < 1) ;; probability of r-base switching
  ex: 
  (pp (r-skull 12 {:complexity 1/6 :poly-prob {2 1, 3 0.8, 5 0.5} :poly-homogeneity 0.3}))"
  [len
   {cmplx :complexity 
    pp    :poly-prob
    ph    :poly-homogeneity
    :or {cmplx 1/4 
         pp {2 1 3 1} 
         ph 0.5 }}]
  (let [p-zones (poly-zones cmplx pp)
        homo? (count= p-zones 1)]
    (if homo? ;if not polyrythmic
      (let [base (first (keys p-zones))
            unit (:unit (p-zones base))] 
        [{:base base :unit unit :len len}])
      (doto-until 
        #(<= len (r-skull-len %))
        (fn [skull]
          (let [p-zones (apply-ph-prob ph skull p-zones)]
            (append-next skull p-zones len)))
        []))))

(comment 
  (r-skull 
    12 
    {:complexity 1/6 
     :poly-prob{2 1 3 0.5 5 0.5} 
     :poly-homogeneity 0.6}))

(comment 
  (r-skull 
    12 
    {:complexity 1/6 
     :poly-prob{3 1} 
     :poly-homogeneity 0.6}))

;;;;;;;;;;;;;;; skull-fill-helpers ;;;;;;;;;;;;;;;;;;

(defn- expand-skull 
  "return seq of all units of a skull
  ex: (expand-skull [{:len 2 :unit 1/4 :base 2} {:len 1 :unit 1/3 :base 3}])
  => (1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/3 1/3 1/3)"
  [skull]
  (repeater 
    (map (fn [{l :len u :unit}] [(/ l u) u]) 
         skull)))

(defn- apply-polar-prob 
  "apply polarity probability to potentialy chosen durations"
  [pol durs pos]
  (let [dur-denom-h (reduce #(assoc %1 %2 (denom (+ pos %2))) {} durs) ;have to include grid here? (pos+ pos %)?
        action (cond 
                  (< pol 0.5) :neg
                  (> pol 0.5) :pos
                  :else :neutral)
        factor (scale-range (abs (- pol 0.5)) 0.5 0 0 1)]
    (case action 
      :pos (map-vals #(/ (* factor %)) dur-denom-h)
      :neg (map-vals #(* factor %) dur-denom-h)
      :neutral (zipmap durs (repeat 1)))))

(defn- choose-dur 
  "choose the next duration in a skull"
  [skull pos params]
  (let [durs (vec (reductions + skull))
        ;center is the closest duration of the mean-speed
        center (closest (:mean-duration params) durs)
        center-idx (.indexOf durs center)
        ;convert homogeneity to agitation
        agitation (abs (- (:homogeneity params) 1))
        side-size (->> (take-while (p > center) durs)
                       count 
                       (* agitation)
                       round)
        possibles (take (inc (* 2 side-size))(subvec durs (- center-idx side-size)))
        chosen (weight-pick-one (apply-polar-prob (:polarity params) possibles pos))
        durs-rest (drop-while (p > chosen) durs)
        skull-rest (steps durs-rest)]
    ; (pp (reduce + skull) [chosen skull-rest])
    [chosen skull-rest]))

;;;;;;;;;;;;;;;;;;; skull-fill ;;;;;;;;;;;;;;;;;;;;;;

(defn skull-fill
  "given a skull and some options, 
  return a seq of durations that fit on the skull accordingly to options
  options: 
  - mean-duration: mean duration!
  - homogeneity: ability of duration to vary from mean-duration
  - polarity: probability of a duration to occur on a strong subdivision" 
  [skull
   {ms :mean-duration
    hom :homogeneity
    pol :polarity
    :as params}]
  (loop [ret []
         skull (expand-skull skull)]
    (let [pos (reduce + ret)
          [chosen skull-rest] (choose-dur skull pos params)
          next-ret (conj ret chosen)]
      (if (seq skull-rest)
        (recur next-ret skull-rest)
        next-ret))))




