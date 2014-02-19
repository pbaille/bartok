(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use bartok.types.w-mode)
  (:use [utils.all :exclude [median]]))

(def ^:private lower-interval-limits
  {:m2 :E-1
   :M2 :Eb-1
   :m3 :C-1
   :M3 :Bb-2
   :P4 :Bb-2
   :+4 :Bb-2
   :P5 :Bb-3
   :+5 :G-2
   :M6 :F-2
   :m7 :F-2
   :M7 :F-2})

(def ^:private b9s 
  (range 13 127 12)); => (13 25 37 49 61 73 85 97 109 121)

;;;;;;;;;;;;;; drop preds ;;;;;;;;;;;;;;;;;;;;

(defn b9-free? 
  "return true if chord has no b9s (see b9s def above) in its inner voices
  means that b9 like intervals are allowed from Bass"
  [[bass frst & nxt :as chord]]
  (if-not (seq nxt)
    true
    (let [[x :as dists] (map #(- % frst) nxt)
          res (if (= 1 x) 
                true ; if first dist = 1 b9 is allowed (bill evans trick) 
                ;if-not check if dists contains no b9 
                (not (some (set dists) b9s)))] 
      (and res (b9-free? (next chord))))))

(defn no-m2-on-top? 
  "check if a drop havn't a m2 between two top notes"
  [d] 
  (not= 1 (last (steps d))))

(defn size=? 
  "return a function that check if drop is of size siz"
  [siz]
  (fn [drop] 
    (= (- (last drop) (first drop)) 
       siz)))

(defn make-drop-pred 
  "take a function on a drop 
  return a function that check if (comparator (fun drop) value) for all comp-val-duplets
  ex: (let [grav-check (make-drop-pred grav-center > 0.5 < 0.8)] 
        (grav-check [0 15 20]) ;=> true 
        (grav-check [0 5 20])  ;=> false)"
  [fun & [comparator value & comp-val-duplets]]
  (fn [drop] 
      (and 
       (comparator (fun drop) value)
       (if (second comp-val-duplets)
         ((ap make-drop-pred fun comp-val-duplets) drop)
         true))))

;;;;;;;;;;; drop analysis ;;;;;;;;;;;;;;;;;

(defn median 
  ([drop] 
    (/ (reduce + drop) (count drop)))
  ;pred constructor for use in drop validator
  ([compar value & comp-val-duplets]
    (ap make-drop-pred median compar value comp-val-duplets)))

(defn grav-center 
  "ex: (grav-center [0 3 10]) => 0.3"
  ([[fi & nxt :as drop]]
    (let [cnt (count drop)
          amplitude (- (last drop) fi)
          from-zero (map #(- % fi) drop)
          inner-sum (reduce + (next (butlast from-zero)))]
      (float 
        (/ (/ inner-sum (- cnt 2))
           amplitude))))
  ;pred constructor for use in drop validator
  ([compar value & comp-val-duplets]
    (ap make-drop-pred grav-center compar value comp-val-duplets)))

(defn mean-interval 
  "ex: (mean-interval [1 2 3 4]) => 1"
  ([[fi :as drop]]
    (/ (- (last drop) fi) (dec (count drop))))
  ;pred constructor for use in drop validator
  ([compar value & comp-val-duplets]
    (ap make-drop-pred mean-interval compar value comp-val-duplets)))

(defn uniformity 
  "repartition uniformity, 0 is perfect uniformity
   ex : (uniformity [1 2 3 4 5]) => 0 "
  ([drop]
    (let [mean-inter (mean-interval drop)
          mean-dists (map #(abs (- mean-inter %)) (steps drop))]
      (median mean-dists)))
  ;pred constructor for use in drop validator
  ([compar value & comp-val-duplets]
    (ap make-drop-pred uniformity compar value comp-val-duplets)))

;;;;;;;;;;;;; Validator ;;;;;;;;;;;;;;;

(defn- drop-validator 
  "return a validator function for drops"
  [& preds]
  (fn [drop]
    (a satisfies-all? drop preds)))

(def ^:private default-validator 
  (drop-validator b9-free? no-m2-on-top?))

;;;;;;;;;;;; options ;;;;;;;;;;;;;;;;;

(def ^:private default-options
  {:max-step 9 
   :max-size 36 
   :inversions false 
   :validator default-validator})

(defn- merge-with-default-options [opt]
  (if-nil-merge opt default-options))

;;;;;;;;;; occ-map related ;;;;;;;;;;;

(defn- occ-map->seq 
  "(occ-map->seq {:M2 2 :m3 1 :M6 2 :M7 1}) 
   => (2 3 9 11 14 21)"
  [om] 
  (sort 
    (mapcat 
      (fn [[k v]]
        (for [i (range v)] 
          (+ (* i 12) (:val (b> k))))) 
      om)))

(defn- occ-repartition 
  "return a occ repartition vector
   ex: (occ-repartition 5 11) => [3 2 2 2 2]"
  [size n]
  (loop [acc (fill-with [] size 0) i 0] 
    (if (< i n) 
      (recur (update-in acc [(mod i size)] inc) (inc i)) 
      acc)))

(b-multi build-occ-map 
  (['ModeClass :number][mode-class n-voices]
    (let [main-degs (mapv :name (:main-degrees (w-mode-class mode-class)))
          size (inc (count main-degs)) ;inc it because :P1 is implicit
          occs (occ-repartition size n-voices)]
      (zipmap (conj main-degs :P1) occs)))
  ([['CIntervalClass] :number][degrees n-voices]
    (let [size (inc (count degrees)) ;inc it because :P1 is implicit
          occs (occ-repartition size n-voices)]
      (zipmap (conj (mapv :name degrees) :P1) occs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- chord-inversions 
  "takes a drop and returns its inversions
  ex:
  (chord-inversions [0 2 3 9 14])
  => [[0 2 3 9 14] [2 3 9 12 14] [3 9 12 14 26] [9 12 14 15 26]]"
  [d]
  (iterate-while
    #(<= (second (last %)) 12) 
    (fn [acc]
      (let [[fl & others :as l] (last acc)
            oc (loop [x (+ fl 12)] 
                 (if (in? others x) (recur (+ 12 x)) x))]
        (conj acc (vec (sort (conj others oc))))))
    [d]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- drops 
  "return a seq of all possible drops of a chord
   while respecting max-step and max size
   ex: (drops [1 3 7 9] 10 24) 
   => ((1 7 15 21) (1 3 9 19) (1 3 7 9) (1 7 9 15) (1 9 15 19))"
  ([coll max-step max-size] (drops coll max-step max-size default-validator))
  ([coll max-step max-size validator] 
    ;validator composed with max-step and max-size constraints                                 
    (letfn [(composed-validator [adrop] 
              (and (every? (p >= max-step) (steps adrop))
                   (<= (last adrop) max-size)
                   (validator adrop)))]
      (if (composed-validator coll)
        ;coll is valid drop so append it to the results
        (cons (seq coll) (drops coll 0 max-step max-size composed-validator))
        ;coll is not valid so continue without keeping it
        (drops coll 0 max-step max-size composed-validator))))
  ([coll idx max-step max-size validator] 
  (let [[head [x y z & others]] (split-at idx coll)
        octaved-up 
        ;find first possible 'octavation'
        (loop [oc (+ y 12)] 
          (if (in? others oc) (recur (+ 12 oc)) oc))]
    (if-not z 
      (list) ;no possible continuation
      ;else continue
      (if (and (<= (- z x) max-step) (<= octaved-up  max-size))
        ; drop is allowed so build it
        (let [ret (concat head (->> (vector x z others octaved-up) (remove nil?) flatten sort))]
          (if (validator ret) 
            ;drop is valid so keep it and continue
            (cons ret (concat (drops ret idx max-step max-size validator) 
                              (drops coll (inc idx) max-step max-size validator)))
            ;drop isn't valid so continue without keeping it
            (concat (drops ret idx max-step max-size validator) 
                    (drops coll (inc idx) max-step max-size validator))))
        ;drop is not allowed so recur with (inc idx)
        (drops coll (inc idx) max-step max-size validator))))))

;;;;;;;;;;;;;;; public ;;;;;;;;;;;;;;;;

(defn all-drops 
  "compute all possible drops of a chord
  args:
  occ-map {c-int-class occurence ...}
  options (optional) {:max-step _ :max-size _ :inversions _ :validator _}
  ex:
  (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 
             {:max-step 7 
              :max-size 48
              :inversions true
              :validator (drop-validator b9-free? no-m2-on-top?)})"
  ([occ-map] (all-drops occ-map {}))
  ([occ-map options]
    (let [{:keys [max-step max-size inversions validator]} 
          (merge-with-default-options options)]
      (if inversions 
        (let [invs (chord-inversions (occ-map->seq occ-map))]
          (map #(drops % max-step (+ max-size (first %)) validator) invs))
        (drops (occ-map->seq occ-map) max-step max-size validator)))))

(b-multi voicings
  "return a list of [Pitch] voicings accordingly with args..."
  ;;; no-options dispatchs ;;;
  ([['Pitch] 'ModeClass :number] [[bass top] modc n-voices]
    (voicings [bass top] (build-occ-map modc n-voices) default-options))
  ([['Pitch] :map] [[bass top] occ-map]
    (voicings [bass top] occ-map default-options))
  (['Pitch :map] [root-pitch occ-map] 
    (voicings root-pitch occ-map default-options))
  ([['Pitch] ['CIntervalClass] :number] [[bass top] degrees n-voices]
    (voicings [bass top] (build-occ-map degrees n-voices default-options)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ([['Pitch] 'ModeClass :number :map] [[bass top] modc n-voices options]
    (voicings [bass top] 
              (build-occ-map modc n-voices) 
              (merge-with-default-options options)))
  
  ([['Pitch] ['CIntervalClass] :number :map] [[bass top] degrees n-voices options]
    (voicings [bass top] 
              (build-occ-map degrees n-voices) 
              (merge-with-default-options options)))
  
  (['Pitch :map :map] [root-pitch occ-map options]
    (let [{invs :inversions :as options} 
          (merge-with-default-options options)
          c-int-map (zipmap (map (f> b> :val)(keys occ-map))
                            (keys occ-map))
          drops (if invs 
                  (a concat (all-drops occ-map options))
                  (all-drops occ-map options))]
      (map (p map 
              #(transpose 
                 root-pitch 
                 (c-interval (c-int-map (mod12 %)) 
                             (int-div % 12)))) 
           drops)))
  
  ([['Pitch] :map :map] [[bass top] occ-map options]
    (let [{:keys [validator] :as options} 
          (merge-with-default-options options)
          size (distance bass top)
          options (-> options 
                      (assoc :max-size size 
                             :validator #(and (validator %) ((size=? size) %))))]
      (voicings bass occ-map options))))

(comment 
  (map (p map :name) (voicings [:Bb-2 :D1] :Mix 8))
  (map (p map :name) (voicings [:Bb-2 :D1] :Mix 8 {:validator (constantly true)}))
  (map (p map :name) (voicings [:Bb-2 :Eb1] [:P4 :M6 :M2 :m7 :P5] 8))
  (map (p map :name) (voicings [:Bb-2 :Eb1] [:P4 :M6 :M2 :m7 :P5] 8 {:max-step 9}))
  (map (p map :name) (voicings [:Bb-2 :D0] {:P1 1 :M3 1 :P4 1 :M6 1 :m7 1}))
  (map (p map :name) (voicings [:Bb-2 :Eb0] {:P1 1 :M2 1 :M3 1 :P4 1 :M6 1 :m7 1} {:validator (constantly true)}))
  (map (p map :name) (voicings :Bb-2 {:P1 1 :M3 1 :P4 1 :M6 1 :m7 1}))
  (map (p map :name) (voicings :Bb-2 {:P1 1 :M2 1 :M3 1 :P4 1 :M6 1 :m7 1} {:validator (constantly true)})))

;;;;;;;;;;;;;; examples ;;;;;;;;;;;;;;;

(comment 
  (time (count (drops [0 2 3 9 11 14 15 21 23] 11 48 (drop-validator b9-free? no-m2-on-top?))))
  (all-distinct? (drops [0 2 3 9 11 14 15 21 23] 11 48)))

(use 'bartok.midi.midi)

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 
                    {:max-step 7 
                     :max-size 48
                     :inversions true})
         rand-nth
         shuffle 
         first
         (map #(pitch (+ 36 %))))))

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :+5 2 :+4 2 :M3 2 :M7 2}
                    {:validator (drop-validator 
                                  b9-free? 
                                  no-m2-on-top? 
                                  (size=? 30))})
         shuffle 
         first
         (map #(pitch (+ 32 %))))))

(comment 
  (play-chord
    (->> (voicings [:C-1 :D#1]
           {:P1 1 :M6 1 :+4 1 :M3 1 :#2 1 :M7 1} 
           {:validator (drop-validator b9-free? no-m2-on-top?)})
         shuffle 
         first)))

(comment 
  (play-chord
    (->> (voicings :Bb-2 {:P1 1 :+4 1 :M6 1 :m7 1 :M2 1} {:max-step 11})
         shuffle 
         first)))



(uniformity [1 2 3 4 50])

