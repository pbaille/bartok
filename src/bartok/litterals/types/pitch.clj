(in-ns 'bartok.litterals.all)

(load "types/pitch_class")

(def pitches 
  (reduce conj #{}      
    (for [{pcn :name npc :natural pca :alteration} pitch-classes 
           oct (range -6 7)]
      (let [val (+ (* 12 (+ oct 5)) (:pitch-val npc) (:val pca) )]
        (when (between val 0 127 )
          (with-type 
            'Pitch
            {:name (keyword (str (name pcn) oct))
             :val val
             :octave oct
             :pitch-class (pitch-class pcn)}))))))

; pitches with either no or b alteration
(def default-name-pitches 
  (let [defaults (set pitch-class-defaults-names)]
    (filter #( defaults (get-in % [:pitch-class :name])) pitches)))

(def name->pitch (reduce #(into %1 {(:name %2) %2}) {} pitches))
(def val->pitch  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitches))

;***************** Constructor ********************

(defn- build-pitch [n v o pc]
  (with-type 'Pitch {:name n :val v :octave o :pitch-class pc}))

; (defmulti pitch b-types)

(b-construct pitch 
             
  [:number v] (val->pitch v)
  [:pitch n] (name->pitch n)
  
  ['PitchClass pc] 
    (pitch (keyword-cat (:name pc) "0"))
  
  ['NaturalPitchClass npc :number n]
    (let [[oct mod] (div-mod n 12)
           alt (- mod (:pitch-val npc))
           possible-alt? (between alt -2 2)]
       ; (debug-repl)
       (when possible-alt? (pitch (pitch-class npc alt) (- oct 5))))  
    
  ['PitchClass p :number o]
    (pitch (keyword-cat (:name p) (str o)))
  
  ['Mode m :number n]
    (let [[oct n-mod] (div-mod n 12)
          [[nam dist][nam2 dist2]] 
          (sort-by #(abs (second %)) (map (juxt :name #(- n-mod (:val %1))) (:pitch-classes m)))
          [p1 p2] [(pitch nam (- oct 5)) (pitch nam2 (- oct 5))]]
      ; (debug-repl)
      (if (zero? dist) 
        p1 
        (or (pitch (-> p1 :pitch-class :natural :name) n) (pitch (-> p2 :pitch-class :natural :name) n))))
    
  ; [:number n 'Mode m]
  ;   (let [n-mod (mod12 n)
  ;         mnpcv (pev (map (c :pitch-val :natural) (:pitch-classes m)))])

)

;**************** functions ******************

(defn distance [p1 p2]
  {:pre [(and (type= p1 'Pitch) (type= p2 'Pitch))]}
  (abs (- (:val p1) (:val p2))))

(defn is-alteration-of [p1 p2]
  {:pre [(and (type= p1 'Pitch) (type= p2 'Pitch))]}
  (and (= (-> p1 :pitch-class :natural) (-> p2 :pitch-class :natural))
       (= (:octave p1) (:octave p2))))

; (defn alt [p n] 
;   (let [a (alteration n)
;         int-kw (keyword-cat (:name a) "1-u")]
;     (transpose p (interval int-kw))))
        
(defmethod transpose ['Pitch 'Interval] [this interval]
    (let [pc (transpose (:pitch-class this) interval)
          v (+ (:val this) (:val interval))
          o (+ (:octave this) (:octave-offset interval))
          o (if (< (-> pc :natural :val) (-> this :pitch-class :natural :val)) (inc o) o)
          n (keyword-cat (:name pc) (keyword (str o)))]
      (build-pitch n v o pc)))
