; REPLACED BY POSITION

; (ns bartok.structure.grid
;   (:use utils.utils)
;   (:use utils.interpolator)
;   (:use bartok.rythmn.rval)
;   (:use [bartok.litterals.evaluation]))

; ; functions arguments: 
; ; g => Grid
; ; p => Position
; ; n => Note

; (defn position [cycl bar sub]
;   (with-type 'Position {:cycle cycl :bar bar :sub sub}))

; (def ^:private default-grid
;   (with-type 
;     'Grid
;     {:bars (b> [:4|4])
;      :tempo [[0 4 60]]
;      :position (position 0 0 0)}))

; (defn grid [m] (conj default-grid m))

; (defn set-position [g p]
;   (conj g {:position p}))

; (defn set-sub [g s]
;   (conj g {:position (conj (:position g) {:sub s})}))

; (defn- bar-inc [g] 
;   (let [pos (conj (:position g) {:bar (-> g :position :bar inc)})]
;     (if (< (:bar pos) (count (g :bars)))
;       (set-position g pos)
;       (set-position g (conj pos {:cycle (-> pos :cycle inc) :bar 0})))))

; (defn- bar-dec [g] 
;   (let [pos (conj (:position g) {:bar (-> g :position :bar dec)})]
;     (if (>= (:bar pos) 0)
;       (set-position g pos)
;       (set-position g (conj pos {:cycle (-> pos :cycle dec) :bar (-> g :bars count dec)})))))

; (defn current-bar [g]
;   (-> g :bars (nth (-> g :position :bar))))

; (defn previous-bar [g]
;   (-> g :bars (nth (-> g :position :bar dec (mod (count (:bars g)))))))

; (defn cycle-val [g]
;   (reduce + (map :val (:bars g))))

; (defn previous-bars-val [g]
;   (reduce + (take (-> g :position :bar) 
;                   (->> g :bars (map :val)))))

; (defn position-add [g rval]
;   (let [sub (+ rval (-> g :position :sub))
;         current-bar-val (:val (current-bar g))]
;     (cond 
;       (>= sub current-bar-val)
;         (position-add 
;           (bar-inc (set-sub g 0)) 
;           (- sub current-bar-val))
;       (neg? sub)
;         (position-add 
;           (bar-dec (set-sub g (-> g previous-bar :val))) 
;           (+ sub (-> g :position :sub)))
;       :else  
;         (set-sub g sub))))

; (defn position-val 
;   ([g]
;     (let [{:keys [cycle bar sub]} (:position g)]
;       (+ (* (cycle-val g) cycle) (previous-bars-val g) sub)))
;   ([g p] (position-val (set-position g p))))

; (defn tempo-interpolator [g] 
;   (cyclic-interpolator (:tempo g) (cycle-val g)))

; (defn note-to-ms [g n]
;   (let [dur (:duration n)
;         pos-val (->> n :position (position-val g))
;         med-tempo ((tempo-interpolator g) pos-val (+ pos-val dur))]
;     (to-ms dur med-tempo)))

