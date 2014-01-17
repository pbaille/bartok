(in-ns 'bartok.litterals.types)

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

(defmulti pitch b-types)

(defmethod pitch :number [v] (val->pitch v))
(defmethod pitch :pitch [n] (name->pitch n))

(defmethod pitch 'PitchClass [pc] 
  (pitch (keyword-cat (:name pc) "0")))

; (defmethod pitch ['NaturalPitchClass :number] [npc n]
;   (let [[oct mod] (div-mod n 12)
;         alt (- mod (:pitch-val npc))
;         possible-alt? (between alt -2 2)]
;     (debug-repl)
;     (when possible-alt? (pitch (pitch-class npc alt) (- oct 5)))))

(defmethod pitch [:pitch-class :number] [p o]
  (pitch (keyword-cat p (str o))))

(defmethod pitch ['PitchClass :number] [p o]
  (pitch (keyword-cat (:name p) (str o))))

; (defmethod pitch :default [& args] 
;   (a pitch (bartok.litterals.evaluation/b> args)))

; (defmethod pitch [:number 'Mode] [n m]
;   (let [n-mod (mod12 n)
;         mnpcv (pev (map (c :pitch-val :natural) (:pitch-classes m)))]))

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
