(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use bartok.types.w-mode)
  (:use [utils.all :exclude [median]]))

;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;

  (def ^:private lower-interval-limits
    "assoc each interval to his corresponding lower-limit"
    {:P1 :C-5
     :m2 :F-1
     :M2 :F-1
     :m3 :Eb-1
     :M3 :D-1
     :P4 :Eb-1
     :+4 :E-1
     :P5 :F-2
     :+5 :Eb-1
     :M6 :D-1
     :m7 :Eb-1
     :M7 :E-1})

  (def ^:private int-lil 
    "integer version of lower-interval-limits"
    (map-h* (c :val b>) lower-interval-limits))

  (def ^:private b9s 
    (range 13 127 12)); => (13 25 37 49 61 73 85 97 109 121)

;;;;;;;;;;;; drop preds ;;;;;;;;;;;;;;

  (defn lower-limits-ok? 
    "check if a drop doesn't exceed lower-interval-limits with given root-val"
    [root-val drop]
    (->> (map (juxt (f> c-interval-class :val) (p + root-val)) drop)
         (every? (fn [[x y]] (<= (int-lil x) y)))))

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
    "take a function 'fun' on a drop 
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

  ;; buggy, try to make a macro for avoiding multiple evaluation of (fun drop) ...
  ; (defmacro make-drop-pred 
  ;   [fun & comp-val-duplets]
  ;   `(fn [x#] 
  ;      (let [res# (~fun x#)] 
  ;        (and ~@(map (fn [[c v]] (list c res# v)) (partition 2 comp-val-duplets))))))

;;;;;;;;;;; drop analysis ;;;;;;;;;;;;

  (defn amplitude
    ([drop] 
      (- (last drop)(first drop)))
    ;pred constructor for use in drop validator
    ([compar value & comp-val-duplets]
      (ap make-drop-pred amplitude compar value comp-val-duplets)))

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

;;;;;;;;;;;;; Validator ;;;;;;;;;;;;;;

  (defn drop-validator 
    "return a validator function for drops"
    [& preds]
    (fn [drop]
      (a satisfies-all? drop preds)))
  
  (defn add-validator [validator & preds]
    (ap drop-validator validator preds))
  
  (def ^:private default-validator 
    (drop-validator b9-free? no-m2-on-top?))

  (defn default-validator-and
    "assoc given preds to the default validator defined above"
    [& preds]
    (ap drop-validator default-validator preds))

;;;;;;;;;;;;; options ;;;;;;;;;;;;;;;;

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
  
  (defn- occ-repartitions 
    "same as occ-repartition but return all permutations"
    [size n]
    (c/permutations (occ-repartition size n)))
    
  (b-multi build-occ-map 
    (['ModeClass 'Number][mode-class n-voices]
      (let [main-degs (mapv :name (:main-degrees (w-mode-class mode-class)))
            size (inc (count main-degs)) ;inc it because :P1 is implicit
            occs (occ-repartition size n-voices)]
        (zipmap (conj main-degs :P1) occs)))
    ([['CIntervalClass] 'Number][degrees n-voices]
      (let [size (inc (count degrees)) ;inc it because :P1 is implicit
            occs (occ-repartition size n-voices)]
        (zipmap (conj (mapv :name degrees) :P1) occs))))

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
  
  (['Pitch 'ModeClass 'Number 'Map] [root-pitch modc n-voices options]
    (voicings root-pitch 
              (build-occ-map modc n-voices) 
              (merge-with-default-options options)))
  
  ([['Pitch] 'ModeClass 'Number 'Map] [[bass top] modc n-voices options]
    (voicings [bass top] 
              (build-occ-map modc n-voices) 
              (merge-with-default-options options)))
  
  (['Pitch ['CIntervalClass] 'Number 'Map] [root-pitch degrees n-voices options]
    (voicings root-pitch 
              (build-occ-map degrees n-voices) 
              (merge-with-default-options options)))
  
  ([['Pitch] ['CIntervalClass] 'Number 'Map] [[bass top] degrees n-voices options]
    (voicings [bass top] 
              (build-occ-map degrees n-voices) 
              (merge-with-default-options options)))
  
  (['Pitch 'Map 'Map] [root-pitch occ-map options]
    (let [{invs :inversions :as options} 
          (merge-with-default-options options)
          c-int-map (zipmap (map (f> b> :val)(keys occ-map))
                            (keys occ-map))
          ; setup the lower interval limit constraint
          lil-constraint (p lower-limits-ok? (:val root-pitch))
          ;merge it into options
          options (assoc options :validator 
                    (add-validator (:validator options) lil-constraint))
          drops (if invs 
                  (a concat (all-drops occ-map options))
                  (all-drops occ-map options))]
      (map (p map 
              #(transpose 
                 root-pitch 
                 (c-interval (c-int-map (mod12 %)) 
                             (int-div % 12)))) 
           drops)))
  
  ([['Pitch] 'Map 'Map] [[bass top] occ-map options]
    (let [{:keys [validator] :as options} 
          (merge-with-default-options options)
          size (distance bass top)
          options (-> options 
                      (assoc :max-size size 
                             :validator #(and (validator %) ((size=? size) %))))]
      (voicings bass occ-map options)))
  
  ;little hack to don't have to add empty map as last args when we want default options
  (:default [& args]
    (cond 
      ; if args are bartokized and last arg is default-options then we-ve try everything... no possible dispatch
      (and (= (last args) default-options) (= args (b> args))) 
        (pp "no dispatch value for" (b-types args)) 
      ; if args are already bartokized try to add the default-options
      (= args (b> args)) (a voicings (conj (vec args) default-options))
      ; if args are not yet bartokified then try if it yields to a valid dispatch
      :else (a voicings (b> args)))))

;;;;;;;;;;;;;; examples ;;;;;;;;;;;;;;;

  (comment 
    (map (p map :name) (voicings [:Bb-2 :D1] :Mix 8))
    (map (p map :name) (voicings [:Bb-2 :D1] :Mix 8 {:validator (constantly true)}))
    (map (p map :name) (voicings [:Bb-2 :Eb1] [:P4 :M6 :M2 :m7 :P5] 8))
    (map (p map :name) (voicings [:Bb-2 :Eb1] [:P4 :M6 :M2 :m7 :P5] 8 {:max-step 9}))
    (map (p map :name) (voicings [:Bb-2 :D0]  {:P1 1 :M3 1 :P4 1 :M6 1 :m7 1}))
    (map (p map :name) (voicings [:Bb-2 :Eb0] {:P1 1 :M2 1 :M3 1 :P4 1 :M6 1 :m7 1} {:validator (constantly true)}))
    (map (p map :name) (voicings :Bb-2 {:P1 1 :M3 1 :P4 1 :M6 1 :m7 1}))
    (map (p map :name) (voicings :Bb-2 {:P1 1 :M2 1 :M3 1 :P4 1 :M6 1 :m7 1} {:validator (constantly true)})))

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
           rand-nth
           (map #(pitch (+ 36 %))))))

  (comment 
    (play-pitch-line 
      (->> (all-drops {:P1 2 :+5 2 :+4 2 :M3 2 :M7 2}
                      {:validator (default-validator-and 
                                    (grav-center > 0.5))})
           rand-nth
           (map #(pitch (+ 44 %))))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          [:C-1 :D#1]
          {:P1 1 :M6 1 :+4 1 :M3 1 :#2 1 :M7 1} 
          {:validator (default-validator-and
                        (grav-center > 0.5))}))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :Ab-2 
          {:P1 1 :P5 1 :P4 1 :M6 1 :m7 1 :M2 1 :M3 1} 
          {:max-size 48
           :validator (default-validator-and
                        (grav-center > 0.6))}))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :C-1 
          [:M3 :M2 :P5 :M7] 6))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :Bb-2  
          [:M6 :P4 :M2 :P5 :m7] 6))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :Eb-1  
          [:M3 :M2 :P5 :M7] 6))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :Ab-2  
          [:M6 :P4 :M2 :P5 :m7] 6
          {:validator (default-validator-and 
                        (amplitude or= [29 33]) 
                        (median > 12))}))))

  (comment 
    (play-chord
      (rand-nth 
        (voicings 
          :Db-2  
          [:M3 :+4 :M6 :P5 :M7] 6))))

;;;;;;;;;;;;; voice-leading wip ;;;;;;;

  (def a-voice-leading
    (let [v1 (voicings 
              :Db-2  
              [:M3 :+4 :M6 :P5 :M7] 6)
          v2 (voicings 
              :Ab-2  
              [:M6 :P4 :M2 :P5 :m7] 6
              {:validator (default-validator-and 
                            (amplitude or= [29 33]) 
                            (median > 12))})
          leadings (map next 
                     (sort-by first 
                       (for [x1 v1 x2 v2] 
                         [(reduce + (map distance  x1 x2)) x1 x2])))]
      leadings))

  (defn best-voice-leading [chord others]
    (second 
      (first 
        (map next 
          (sort-by first 
            (for [x others] 
              [(reduce + (map distance  chord x)) chord x]))))))

  (defn play-nth-voice-leading [n]
    (play-pitch-line 
      [(play-chord (first  (nth a-voice-leading n)))
       (play-chord (second (nth a-voice-leading n)))]
      {:duration 4}))

  (comment 
    (play-pitch-line 
    [(first (first a-voice-leading))(second (first a-voice-leading))]
    {:duration 4}))

  (defn do-voice-leading [fst & chords-colls]
    (reduce 
      #(conj %1 (best-voice-leading (last %1) %2))
      [(rand-nth fst)]
      chords-colls))

  (comment 
   (play-pitch-line
   (do-voice-leading 
    (voicings :C-2  [:M7 :M2 :P5 :M3] 10 {:max-step 9 :max-size 48 :validator (default-validator-and (grav-center > 0.6) (mean-interval < 7))})
    (voicings :Bb-2 [:M6 :M3 :M2 :P5 :m7] 6)
    (voicings :Eb-2 [:M7 :M2 :P5 :M3] 5)
    (voicings :Ab-2 [:M6 :M3 :M2 :P5 :m7] 6)
    (voicings :Db-1 [:M2 :M3 :M7 :M6 :P5] 6)
    (voicings :G-2  [:M6 :M3 :m2 :m7] 6)
    (voicings :C-1  [:M2 :M3 :M7 :M6 :P5] 6)
    ; (voicings :Bb-2 [:M6 :M2 :+4 :m7 :M3] 6)
    ; (voicings :D-1  [:M7 :M2 :P5 :M3] 6)
    ; (voicings :A-2  [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :F#-2 [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :B-2  [:M6 :m2 :m7 :M3] 6)
    ; (voicings :E-2  [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :Ab-2 [:M6 :P4 :M2 :P5 :m7] 6)
    ; (voicings :Db-2 [:M7 :M2 :P5 :M3] 6)
    ; (voicings :G-2  [:M6 :P4 :M2 :P5 :m7] 6)
    
    ; (voicings :C-1  [:M7 :M2 :P5 :M3] 6)
    ; (voicings :Bb-2 [:M6 :P4 :M2 :P5 :m7] 6)
    ; (voicings :Eb-2 [:M7 :M2 :P5 :M3] 6)
    ; (voicings :Ab-2 [:M6 :P4 :M2 :P5 :m7] 6)
    ; (voicings :Db-2 [:M7 :M2 :P5 :M3] 6)
    ; (voicings :G-2  [:M6 :P4 :M2 :P5 :m7] 6)
    ; (voicings :C-1  [:M7 :M2 :P5 :M3] 6)
    ; (voicings :Bb-2 [:M6 :M2 :+4 :m7 :M3] 6)
    ; (voicings :D-1  [:M7 :M2 :P5 :M3] 6)
    ; (voicings :A-2  [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :F#-2 [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :B-2  [:M6 :m2 :m7 :M3] 6)
    ; (voicings :E-2  [:P4 :M2 :P5 :m7 :m3] 6)
    ; (voicings :Ab-2 [:M6 :P4 :M2 :P5 :m7] 6)
    ; (voicings :Db-2 [:M7 :M2 :P5 :M3] 6)
    ; (voicings :G-2  [:M6 :P4 :M2 :P5 :m7] 6)
    )
   {:duration 4}))

  ; (play-chord (rand-nth (voicings :C-1 (chord-class 'M7.#9.m6) 6)))

;;;;;;;;;;;;; multi-occ-maps ;;;;;;;;;;



  (b-umulti build-occ-maps
    ['ModeClass mode-class 'Number n-voices]
      (let [main-degs (mapv :name (:main-degrees (w-mode-class mode-class)))
            size (inc (count main-degs)) ;inc it because :P1 is implicit
            occs (occ-repartitions size n-voices)]
        (map #(zipmap (conj main-degs :P1) %) occs))
    ['ModalStructClass mode-class 'Number n-voices]
      (let [main-degs (mapv :name (:main-degrees (w-mode-class mode-class)))
            size (inc (count main-degs)) ;inc it because :P1 is implicit
            occs (occ-repartitions size n-voices)]
        (map #(zipmap (conj main-degs :P1) %) occs))
    [['CIntervalClass] degrees 'Number n-voices]
      (let [size (inc (count degrees)) ;inc it because :P1 is implicit
            occs (occ-repartitions size n-voices)]
        (map #(zipmap (conj (mapv :name degrees) :P1) %) occs)))

  (defn all-drops* 
    "compute all possible drops of a chord
    args:
    occ-map {c-int-class occurence ...} (multiple occ-map(s) can be passed in a vector)
    options (optional) {:max-step _ :max-size _ :inversions _ :validator _}
    ex:
    (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 
               {:max-step 7 
                :max-size 48
                :inversions true
                :validator (drop-validator b9-free? no-m2-on-top?)})"
    ([occ-map] (all-drops* occ-map {}))
    ([occ-map options]
      (let [{:keys [max-step max-size inversions validator]} 
            (merge-with-default-options options)
            occ-map (vec-if-not occ-map)]
        (if inversions 
          (let [invs (mapcat chord-inversions (map occ-map->seq occ-map))]
            (map #(drops % max-step (+ max-size (first %)) validator) invs))
          (mapcat #(drops (occ-map->seq %) max-step max-size validator) occ-map)))))

  (b-umulti voicings-um
    "return a list of [Pitch] voicings accordingly with args..."
    
    [#{'Pitch ['Pitch]} pcs 'ModeClass modc 'Number n-voices 'Map options]
      (voicings pcs 
                (build-occ-map modc n-voices) 
                (merge-with-default-options options))
    
    [#{'Pitch ['Pitch]} pcs ['CIntervalClass] degrees 'Number n-voices 'Map options]
      (voicings pcs
                (build-occ-map degrees n-voices) 
                (merge-with-default-options options))
      
    [#{'Pitch ['Pitch]} pcs 'ModalStructClass msc 'Number n-voices 'Map options]
      (voicings pcs
                (build-occ-map (:degrees msc) n-voices) 
                (merge-with-default-options options))
    
    ['ModalStruct msc 'Number n-voices 'Map options]
      (voicings (:root ms)
                (build-occ-map (:degrees (:class msc)) n-voices) 
                (merge-with-default-options options))
    
    ['ModalStruct msc 'Pitch top 'Number n-voices 'Map options]
      (voicings [(:root ms) top]
                (build-occ-map (:degrees (:class msc)) n-voices) 
                (merge-with-default-options options))
      
    ['Pitch root-pitch 'Map occ-map 'Map options] 
      (let [{invs :inversions :as options} 
            (merge-with-default-options options)
            c-int-map (zipmap (map (f> b> :val)(keys occ-map))
                              (keys occ-map))
            ; setup the lower interval limit constraint
            lil-constraint (p lower-limits-ok? (:val root-pitch))
            ;merge it into options
            options (assoc options :validator 
                      (add-validator (:validator options) lil-constraint))
            drops (if invs 
                    (a concat (all-drops occ-map options))
                    (all-drops occ-map options))]
        (map (p map 
                #(transpose 
                   root-pitch 
                   (c-interval (c-int-map (mod12 %)) 
                               (int-div % 12)))) 
             drops))
    
    [['Pitch] [bass top] 'Map occ-map 'Map options] 
      (let [{:keys [validator] :as options} 
            (merge-with-default-options options)
            size (distance bass top)
            options (-> options 
                        (assoc :max-size size 
                               :validator #(and (validator %) ((size=? size) %))))]
        (voicings bass occ-map options))
  )

  (defn- occ-map? [om]
    (when (map? om)
      (= (b-type (seq om)) 
         [['c-interval-class 'Number]])))

  (defn- occ-maps? [oms]
    (when (or (vector? oms) 
              (list? oms) 
              (set? oms))
      (every? occ-map? oms)))

  (defn voicings* 
    "need to wrap voicing-um call into this 
    because would like to make the options arg optional,
    little smell but... "
    [& args] 
    (let [l (last args)]
      ; (dr)
      (if (or (not (map? l))
              (occ-map? l)
              (occ-maps? l))
        (a voicings-um (concat args [default-options]))
        (a voicings-um args))))

  (voicings* [:Bb-2 :D1] :Mix 8)
  (play-chord (rand-nth (voicings* [:Bb-2 :E1] :m∆#119 6)))
  
  (coerce-type (do-type #{'Pitch ['Pitch]}) [:C1 :C2])