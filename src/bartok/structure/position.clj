(ns bartok.structure.position
  (:use utils.utils)
  (:use utils.interpolator)
  (:use bartok.rythmn.rval)
  (:use bartok.types))

; functions arguments: 
; g => Grid
; p => Position
; n => Note

(defn position 
  ([grid] (position grid 0 0 0))
  ([grid cycl bar sub]
    (with-type 'Position {:grid grid :cycle cycl :bar bar :sub sub})))

(def ^:private default-grid
  (with-type 
    'Grid
    {:bars [(time-signature :4|4)]
     :tempo [[0 4 60]]}))

(defn grid 
  ([] (grid {}))
  ([m] (conj default-grid m)))

(defn set-position [ pos cycle bar sub]
  (conj pos {:cycle cycle :bar bar :sub sub}))

(defn set-sub [p s]
  (conj p {:sub s}))

(defn- bar-inc [p] 
  (let [pos (conj p {:bar (-> p :bar inc)})]
    (if (< (:bar pos) (count (p :grid :bars)))
      pos
      (conj pos {:cycle (-> pos :cycle inc) :bar 0}))))

(defn- bar-dec [p] 
  (let [pos (conj p {:bar (-> p :bar dec)})]
    (if (>= (:bar pos) 0)
      pos
      (conj pos {:cycle (-> pos :cycle dec) :bar (-> p :grid :bars count dec)}))))

(defn current-bar [p]
  (-> p :grid :bars (nth (-> p :bar))))

(defn previous-bar [p]
  (-> p :grid :bars (nth (-> p :bar dec (mod (count (-> p :grid :bars)))))))

(defn cycle-val [g]
  (reduce + (map :val (:bars g))))

(defn previous-bars-val [p]
  (reduce + (take (:bar p) 
                  (->> p :grid :bars (map :val)))))

(defn position-add [p rval]
  (let [sub (+ rval (-> p :sub))
        current-bar-val (:val (current-bar p))
        _ (p sub)
        _ (p current-bar-val)]
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

(defn tempo-interpolator [g] 
  (cyclic-interpolator (-> g :tempo) (cycle-val g)))

(defn note-to-ms [n]
  (let [dur (:duration n)
        pos-val (->> n :position position-val)
        med-tempo ((tempo-interpolator (-> n :position :grid)) pos-val (+ pos-val dur))]
    (to-ms dur med-tempo)))

