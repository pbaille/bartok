(ns bartok.melody.melodic-domain
  (:use midje.sweet)
  (:use bartok.structure)
  (:use utils.all)
  (:use bartok.primitives))

;;;;;;;;;;;;;;;;;;;; Private ;;;;;;;;;;;;;;;;;;;;;;;;

(defn- midi-octave [x]
  (- (int (/ x 12)) 5))

(defn- current-pitch [md]
  (-> md :current :pitch))

(defn- if-val [x] (or (:val x) x))

(defn- init-and-compute-pitches [mode-or-pcs bounds]
  (let [pcs (if (type= mode-or-pcs 'Mode) (:pitch-classes mode-or-pcs) mode-or-pcs)
        rng (range (-> bounds first :val) 
                   (-> bounds second :val inc))
        mpvs (apply hash-map (mapcat (juxt :val :name) pcs))]
    (with-type 'MelodicDomain 
      {:pitch-classes pcs
       :bounds bounds
       :pitches (vec (for [x rng :when (contains? mpvs (mod12 x))]
                  (pitch (keyword-cat (mpvs (mod12 x)) (str (midi-octave x))))))})))

(defn set-current [md current]
  (let [p (if (type= current 'Pitch)
            (select-first #(is-alteration-of % current)
                  (:pitches md))
            (-> md :pitches first))
        i (index-of p (:pitches md))]
    (assoc md :current {:pitch p :index i})))

(defn- valid-domain-index? [md i]
  (between i 0 (-> md :pitches count dec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(b-fn melodic-domain
  ;[Mode [Pitch Pitch]] -> MelodicDomain 
  ([mode bounds] (melodic-domain mode bounds false))
  ;[Mode [Pitch Pitch] Pitch] -> MelodicDomain 
  ([mode bounds current]
  (let [md (init-and-compute-pitches mode bounds)
        md (set-current md current)]
    md )))

(defn interval-bounds [dom]
  (let [i (-> dom :current :index)]
    [(d-interval (* -1 i)) 
     (d-interval (- (-> dom :pitches count dec) i))]))

;;; ** grid related ** ;;;
(defn global-bounds 
  ([modes bounds start-pitch]
    (let [domains (->> modes (map #(melodic-domain % bounds start-pitch)))
          domains-bounds (map #(map :val (interval-bounds %)) domains)]
      [(best > (map first domains-bounds)) (best < (map second domains-bounds))]))
  ([start-pos end-pos bounds start-pitch]
    (global-bounds (modes-between start-pos end-pos) bounds start-pitch)))

(defn md-amplitude [md]
  (count (:pitches md)))

(defn steps-in-bounds? 
  "return true if the step sequence fit in md when starting from md current pitch"
  [md steps]
  (let [indexes (map-reduce + (-> md :current :index) (map to-num steps))]
    (every? #(valid-domain-index? md %) indexes)))

(defn step 
  "transpose the current pitch by d-int or return nil"
  [md d-int]
  (let [i-val (if (number? d-int) d-int (:val d-int))
        new-index (+ i-val (-> md :current :index))]
    (if-let [curr-pitch (get (:pitches md) new-index)]
      (conj md {:current {:index new-index :pitch curr-pitch}})
      nil)))

(defn step-sequence 
  ;single domain
  ([md steps]
  (let [indexes (map-reduce + (-> md :current :index) (map to-num steps))]
    (when (every? #(valid-domain-index? md %) indexes)
      (map #(-> md :pitches (nth %)) indexes)))) ;with start pitch? (cons (-> md :current :pitch) (map #(-> md :pitches (nth %)) indexes))
  ;[{:mode mode-name :steps [step ...]}...] / [Pitch Pitch] / Pitch
  ([coll bounds start-pitch]
   (reduce
      (fn [acc {:keys [steps mode]}]
        (let [md (melodic-domain mode bounds (or (last acc) start-pitch))
              s (step-sequence md steps)]
          (if (nil? s) (println "halt!!!!! out of bounds step-sequence !!!!"))
          (concat acc s))) 
      [] coll)))

(b-fn c-step 
  "transpose the current pitch by c-int or return nil if out of bounds
  note: if resulted current-pitch is not diatonic: [:current :index] become an array of the 2 closest indexes"
  [md c-int] 
  (let [diat-dist (-> c-int :diatonic :val)
        new-pitch (transpose (current-pitch md) c-int)
        in-mode? (in-mode? new-pitch (:mode md))
        current-index (-> md :current :index)
        current-index (if (vector? current-index) (first current-index) current-index)
        maybe-index (+ diat-dist current-index)]
    (when-let [in-new-pitch (-> md :pitches (nth maybe-index))]
      (let [new-index (if in-mode? 
                    maybe-index
                    (if (b:> in-new-pitch new-pitch)
                      [(dec maybe-index) maybe-index]
                      [maybe-index (inc maybe-index)]))]
        (conj md {:current {:index new-index :pitch new-pitch}})))))

;have to take care of enharmonic errors 
;ex: if (= c-ints (repeat) :4th-u) 
;=> C F Bb Eb Ab Db Gb Cb Fb Bbb Ebb Abb Dbb Gbb Cbb Fbb Bbbb !!! crash
; have to force enharmonic substitution at some point...
(b-fn c-step-sequence 
  ;single domain
  ([md c-ints]
  (let [{pitches :pitches md :md} 
        (reduce (fn [{md :md ps :pitches} ci]
                  (let [nxt (c-step md ci)]
                    (dr)
                    (when nxt {:md nxt :pitches (conj ps (current-pitch nxt))})))
                {:md md :pitches []} 
                c-ints)]
    (when md pitches)))
  ;[{:mode mode-name :steps [step ...]}...] / [Pitch Pitch] / Pitch
  ([coll bounds start-pitch]
   ;TODO
   ))


(fact "melodic-domain"
  (def- md (melodic-domain :C-Lyd [:C0 :C2] :C1))
    (melodic-domain :C-Lyd [:C0 :C2]) => (melodic-domain :C-Lyd [:C0 :C2] :C0)
    (interval-bounds md)
      => [(d-interval :1st-d1) (d-interval :1st-u1)]
    (md-amplitude md) => 15
    (step md 1) => (melodic-domain :C-Lyd [:C0 :C2] :D1)
    (step-sequence md [1 1 1]) => [(b> :D1)(b> :E1)(b> :F#1)])


