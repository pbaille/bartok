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

(defn- init-and-compute-pitches [mode bounds]
  (let [rng (range (-> bounds first :val) 
                   (-> bounds second :val inc))
        mpvs (apply hash-map (mapcat (juxt :val :name) (:pitch-classes mode)))]
    ; (dr)
    (with-type 'MelodicDomain 
      {:mode mode
       :bounds bounds
       :pitches (vec (for [x rng :when (contains? mpvs (mod12 x))]
                  (pitch (keyword-cat (mpvs (mod12 x)) (str (midi-octave x))))))})))

(defn set-current [md current]
  (let [p (if (type= current 'Pitch)
            (select-first #(is-alteration-of % current)
                  (:pitches md))
            (-> md :pitches first))
        i (index-of p (:pitches md))]
    (conj md {:current {:pitch p :index i}})))

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
          (if (nil? s) (pp "halt!!!!! out of bounds step-sequence !!!!"))
          (concat acc s))) 
      [] coll)))


(fact "melodic-domain"
  (def- md (melodic-domain :C-Lyd [:C0 :C2] :C1))
    (melodic-domain :C-Lyd [:C0 :C2]) => (melodic-domain :C-Lyd [:C0 :C2] :C0)
    (interval-bounds md)
      => [(d-interval :1st-d1) (d-interval :1st-u1)]
    (md-amplitude md) => 15
    (step md 1) => (melodic-domain :C-Lyd [:C0 :C2] :D1)
    (step-sequence md [1 1 1]) => [(b> :C1)(b> :D1)(b> :E1)(b> :F#1)])


