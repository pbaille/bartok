(ns bartok.structure.position
  (:use utils.utils)
  (:use utils.interpolator)
  (:use bartok.rythmn.rval)
  (:use bartok.types))

; functions arguments: 
; g => Grid
; p => Position
; n => Note

;******************** grid ***************************

(def ^:private default-grid
  (with-type 
    'Grid
    {:bars [(time-signature :4|4)]
     :tempo [[0 4 60]]}))

(defn- expand-harmonies [g]
  (let [h (:harmony g)
        mh (if (map? h) h (a hash-map h))
        final (map (fn [[k v]] 
                     (hash-map :position k :mode (mode v))) 
                   mh)]
    (assoc g :harmony (sort-by :position final))))

(defn- expand-bars [g]
  (assoc g :bars (map time-signature (repeater (:bars g)))))

(declare position position-val before? after? pos-between?)

(defn grid 
  ([] (grid {}))
  ([m] ((c expand-bars expand-harmonies) (conj default-grid m))))


(defn harmony-at [pos]
  (last (filter #(<= (position-val (assoc pos :bar (-> % :position first) :sub (-> % :position second))) 
                   (position-val pos)) 
                (-> pos :grid :harmony))))

(defn mode-at [pos]
  (:mode (harmony-at pos)))

(defn cycle-val [g]
  (reduce + (map :val (:bars g))))

(defn modes-between [start-pos end-pos]
  (let [[hs he] (map harmony-at [start-pos end-pos])
        g (:grid start-pos)
        cycle-complete? (<= (cycle-val g) (apply - (map position-val [end-pos start-pos])))]
    (if cycle-complete?
      (->> g :harmony (map :mode))
      (let [phsv ((juxt :bar :sub) start-pos) 
            phev ((juxt :bar :sub) end-pos)
            modes (set-map #(-> % :mode) [hs he])]
        (cond 
          (v-lt phsv phev) (reduce #(conj %1 (-> %2 :mode)) 
                                    modes 
                                    (filter (fn [{p :position}] (and (v-gt p phsv) (v-lt p phev))) 
                                            (:harmony g))) 
          (v-gt phsv phev) (reduce #(conj %1 (-> %2 :mode)) 
                                    modes 
                                   (filter (fn [{p :position}] (or (v-lt p phev) (v-gt p phsv))) 
                                           (:harmony g))))))))

(defn tempo-interpolator [g] 
  (cyclic-interpolator (-> g :tempo) (cycle-val g)))

;********************* position **********************

(defn position 
  ([grid] (position grid 0 0 0))
  ([grid cycl bar sub]
    (with-type 'Position {:grid grid :cycle cycl :bar bar :sub sub})))

(defn set-position [ pos cycle bar sub]
  (conj pos {:cycle cycle :bar bar :sub sub}))

(defn set-sub [p s]
  (conj p {:sub s}))

(defn- bar-inc [p] 
  (let [pos (conj p {:bar (-> p :bar inc)})]
    (if (< (:bar pos) (count (-> p :grid :bars)))
      pos
      (conj pos {:cycle (-> pos :cycle inc) :bar 0}))))

(defn- bar-dec [p] 
  (let [pos (conj p {:bar (-> p :bar dec)})]
    (if (>= (:bar pos) 0)
      pos
      (conj pos {:cycle (-> pos :cycle dec) :bar (-> p :grid :bars count dec)}))))

(defn current-bar [pos]
  (-> pos :grid :bars (nth (-> pos :bar))))

(defn previous-bar [p]
  (-> p :grid :bars (nth (-> p :bar dec (mod (count (-> p :grid :bars)))))))



(defn previous-bars-val [p]
  (reduce + (take (:bar p) 
                  (->> p :grid :bars (map :val)))))

(defn position-add [p rval]
  (let [sub (+ rval (-> p :sub))
        current-bar-val (:val (current-bar p))]
    (cond 
      (>= sub current-bar-val)
        (position-add 
          (bar-inc (set-sub p 0)) 
          (- sub current-bar-val))
      (neg? sub)
        (position-add 
          (bar-dec (set-sub p (-> p previous-bar :val))) 
          (+ sub (-> p :sub)))
      :else  
        (set-sub p sub))))

(defn position-val [p]
  (let [{:keys [cycle bar sub]} p]
    (+ (* (cycle-val (:grid p)) cycle) (previous-bars-val p) sub)))

(defn before? [pos1 pos2]
  (pos? (apply - (map position-val [pos2 pos1]))))

(defn after? [pos1 pos2]
  (neg? (apply - (map position-val [pos2 pos1]))))

(defn eq? [pos1 pos2]
  (= 0 (apply - (map position-val [pos2 pos1]))))

(defn pos-between? 
  ([pos [pos1 pos2]] (pos-between? pos pos1 pos2))
  ([pos pos1 pos2]
   (let [[pos1 pos2] (if (before? pos1 pos2) [pos1 pos2] [pos2 pos1])]
     (or (and (after? pos pos1) (before? pos pos2)) 
         (or (eq? pos pos1) (eq? pos pos2))))))

(defn note-to-ms [n]
  (let [dur (:duration n)
        pos-val (->> n :position position-val)
        med-tempo ((tempo-interpolator (-> n :position :grid)) pos-val (+ pos-val dur))]
    (to-ms dur med-tempo)))

(defn pos-to-ms [p]
  (let [dur (position-val p)
        med-tempo ((tempo-interpolator (:grid p)) 0 dur)]
    (to-ms dur med-tempo)))
