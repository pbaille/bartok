(in-ns 'bartok.melody)

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

(defn- add-current [md current]
  (let [p (if (pitch? current)
            (best #(< (distance current %1) (distance current %2))
                  (:pitches md))
            (-> md :pitches first))
        i (index-of p (:pitches md))]
    (conj md {:current {:pitch p :index i}})))

(defn- valid-domain-index? [md i]
  (between i 0 (-> md :pitches count dec)))

(defn melodic-domain 
  ([mode bounds] (melodic-domain mode bounds false))
  ([mode bounds current]
  (let [md (init-and-compute-pitches mode bounds)
        md (add-current md current)]
    md )))

(defn step [md g-interval]
  (let [i-val (if (number? g-interval) g-interval (:val g-interval))
        new-index (+ i-val (-> md :current :index))]
    (if-let [curr-pitch (get (:pitches md) new-index)]
      (conj md {:current {:index new-index :pitch curr-pitch}})
      nil)))

(defn step-sequence [md steps]
  (let [indexes (map-reduce + (-> md :current :index) (map if-val steps))]
    (when (every? #(valid-domain-index? md %) indexes)
      (map #(-> md :pitches (nth %)) indexes))))

 


