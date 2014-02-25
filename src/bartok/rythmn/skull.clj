(ns bartok.rythmn.skull
  (:use [bartok.rythmn rval utils])
  (:use bartok.print)
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

(defn- min-rbases-vals 
  "for each given r-base return the minimum value that is greater than min
  ex: (min-rbases-vals 1/6)
  => (1/4 1/6 1/5 2/7)
  (min-rbases-vals 1/6 [3 7]
  => (1/6 2/7))"
  ([min] (min-rbases-vals min [2 3 5 7]))
  ([min bases]
    (map 
      (fn [baz]
        (cond 
          (< baz min) (doto-until (p <= min) (p * 2) baz)
          (> baz min) (doto-while (p <= min) #(/ % 2) baz)
          :else baz))
      (map / bases))))

; (defn min-rbase-val [min base] 
;   (first (min-rbases-vals min [base])))

(defn- r-skull-work-map 
  [cmplx rb-pm]
  (let [rbases (keys rb-pm)
        rb-min-vals (min-rbases-vals cmplx rbases)
        rb-bin-res (map bin-resolution rb-min-vals)]
    (zipmap 
      rbases 
      (map #(hash-map :prob (/ %1 %3) 
                      :unit %2 
                      :size %3) 
           (vals rb-pm)
           rb-min-vals 
           rb-bin-res))))

(defn- r-skull-len [skull]
  (a + (map :len skull)))

(defn- apply-ph-prob 
  [ph skull work-map]
  (if-not (seq skull)
    work-map
    (let [last-base (:base (last skull))
          last-base-val (work-map last-base)
          other-bases (dissoc work-map last-base)
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
        work-map))))

(defn- append-next [skull work-map len]
  (let [skull-len (r-skull-len skull)
        current-sub (denom skull-len)
        remaining-len (- len skull-len)
        filtered 
        (filt-h 
          #(and 
             (>= remaining-len (:size (val %)))
             (in? (allowed-subs (:size (val %))) current-sub)) 
          work-map)
        prob-map (map-vals :prob filtered)
        chosen (weight-pick-one prob-map)]
    (if (not= chosen (:base (last skull)))
      (conj skull {:base chosen 
                   :unit (:unit (work-map chosen)) 
                   :len (:size (work-map chosen))})
      (conj 
        (vec (butlast skull)) 
        (update-in (last skull) [:len] + (:size (work-map chosen)))))))

;;;;;;;;;;;;;;;;;;; r-skull ;;;;;;;;;;;;;;;;;;;;;;

(defn r-skull 
  "
  len :: (Int) ;; length of the skull 
  (maybe allow ratio too, but more complex implementation) 
  options: (see r-skull-work-map destructuring)
    - complexity :: (ratio) ;; minimum allowed subdivision
    - rb-pm :: ({(r-base :: (Num)) (prob :: (Num))}) ;; probability of each r-base to occur
    - ph :: (0 < (Float) < 1) ;; probability of r-base switching
  ex: 
  (pp (r-skull 12 {:complexity 1/6 :r-bases-prob-map {2 1, 3 0.8, 5 0.5} :poly-homogeneity 0.3}))"
  [len
   {cmplx :complexity 
    rb-pm :r-bases-prob-map 
    ph    :poly-homogeneity
    :or {cmplx 1/4 
         rb-pm {2 1 3 1} 
         ph 0.5 }}]
  (let [work-map (r-skull-work-map cmplx rb-pm)]
    (doto-until 
      #(<= len (r-skull-len %))
      (fn [skull]
        (let [work-map (apply-ph-prob ph skull work-map)]
          (append-next skull work-map len)))
      [])))

(comment 
  (r-skull 
    12 
    {:complexity 1/6 
     :r-bases-prob-map {2 1 3 0.5 5 0.5} 
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
        center (closest (:mean-speed params) durs)
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

(defn- refresh-params [durs params]
  ;do nothing for now
  params)

;;;;;;;;;;;;;;;;;;; skull-fill ;;;;;;;;;;;;;;;;;;;;;;

(defn skull-fill 
  [skull
   {ms :mean-speed
    cont :continuity
    hom :homogeneity
    pol :polarity
    :as params}]
  (loop [ret []
         skull (expand-skull skull)
         params params]
    ; (pp ret)
    (let [pos (reduce + ret)
          [chosen skull-rest] (choose-dur skull pos params)
          next-ret (conj ret chosen)
          refreshed-params (refresh-params next-ret params)]
      (if (seq skull-rest)
        (recur next-ret skull-rest refreshed-params)
        next-ret))))


