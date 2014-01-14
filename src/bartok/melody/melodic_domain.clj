(ns bartok.melody.melodic-domain
  (:use [utils.utils])
  (:use [utils.dom-part])
  (:use [bartok.litterals.all]))

;******************* Private ************************

(defn- midi-octave [x]
  (- (int (/ x 12)) 5))

(defn- current-pitch [md]
  (-> md :current :pitch))

(defn- if-val [x] (or (:val x) x))

(defn- init-and-compute-pitches [mode bounds]
  (let [rng (range (-> bounds first :val) 
                   (-> bounds second :val inc))
        mpvs (apply hash-map (mapcat (juxt :val :name) (:pitch-classes mode)))]
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

;*************************** Public *************************************

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
    [(generic-interval (* -1 i)) 
     (generic-interval (- (-> dom :pitches count dec) i))]))

(defn md-amplitude [md]
  (count (:pitches md)))

(defn step [md g-interval]
  (let [i-val (if (number? g-interval) g-interval (:val g-interval))
        new-index (+ i-val (-> md :current :index))]
    (if-let [curr-pitch (get (:pitches md) new-index)]
      (conj md {:current {:index new-index :pitch curr-pitch}})
      nil)))

(defn step-sequence 
  ;single domain
  ([md steps]
  (let [indexes (map-reduce + (-> md :current :index) (map if-val steps))]
    (when (every? #(valid-domain-index? md %) indexes)
      (map #(-> md :pitches (nth %)) indexes))))
  ;[{:mode mode-name :steps [step ...]}...] / [Pitch Pitch] / Pitch
  ([coll bounds start-pitch]
   (reduce
      (fn [acc {:keys [steps mode]}]
        (let [md (melodic-domain mode bounds (or (last acc) start-pitch))
              s (step-sequence md steps)]
          (if (nil? s) (pp "halt!!!!! out of bounds step-sequence !!!!"))
          (concat acc s))) 
      [] coll)))

 


