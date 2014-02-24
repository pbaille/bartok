(ns bartok.rythmn.skull
  (:use [bartok.rythmn rval utils])
  (:use bartok.print)
  (:use [utils utils prob dom-part macros]))

(defn doto-until 
  "like (first (drop-while (c not pred) (iterate f init))) "
  [pred f init]
  (loop [x init] 
    (if (pred x) 
      x 
      (recur (f x)))))

(defn doto-while
  "like (last (take-while pred (iterate f init)))
  returns nil if (not (pred init))"
  [pred f init]
  (loop [x init fx (f x)]
    (if (pred fx)
      (recur fx (f fx))
      (if (pred x) x nil))))

(defn min-rbases-vals 
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
  ; (dr)
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
    ; (dr)
    (if (not= chosen (:base (last skull)))
      (conj skull {:base chosen :len (:size (work-map chosen))})
      (conj 
        (vec (butlast skull)) 
        (update-in (last skull) [:len] + (:size (work-map chosen)))))))

(defn r-skull 
  "
  len :: (Int) ;; length of the skull 
  (maybe allow ratio too, but more complex implementation) 
  options: (see r-skull-work-map destructuring)
    - complexity :: (ratio) ;; minimum allowed subdivision
    - rb-pm :: ({(r-base :: (Num)) (prob :: (Num))}) ;; probability of each r-base to occur
    - ph :: (0 < (Float) < 1) ;; probability of r-base switching
  ex: 
  (pp (r-skull 12 {:complexity 1/6 :r-bases-prob-map {2 1 3 0.8 5 0.5} :poly-homogeneity 0.3}))"
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
  (pp (r-skull 12 
               {:complexity 1/6 
                :r-bases-prob-map {2 1 3 1 5 1} 
                :poly-homogeneity 0})))
