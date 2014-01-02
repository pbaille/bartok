(ns bartok.structure.grid
  (:use [utils.utils]))

(defn position [cycl bar sub]
  (with-type 'Position {:cycle cycl :bar bar :sub sub}))

(defn grid 
  ([bars]
     (grid bars (position 0 0 0))) 
  ([bars position]
     (with-type 'Grid
       {:bars bars
        :position position})))

(defn- bar-inc [g] 
  (let [pos (conj (:position g) {:bar (-> g :position :bar inc)})]
    (if (< (:bar pos) (count (g :bars)))
      (conj g {:position pos})
      (conj g {:position (conj pos {:cycle (-> pos :cycle inc) :bar 0})}))))

(defn- bar-dec [g] 
  (let [pos (conj (:position g) {:bar (-> g :position :bar dec)})]
    (if (>= (:bar pos) 0)
      (conj g {:position pos})
      (conj g {:position (conj pos {:cycle (-> pos :cycle dec) :bar (-> g :bars count dec)})}))))

(defn- set-sub [g s]
  (conj g {:position (conj (:position g) {:sub s})}))

(defn- current-bar [grid]
  (-> grid :bars (nth (-> grid :position :bar))))

(defn- previous-bar [grid]
  (-> grid :bars (nth (-> grid :position :bar dec (mod (count (:bars grid)))))))

(defn position-add [g rval]
  (let [sub (+ rval (-> g :position :sub))
        current-bar-val (:val (current-bar g))]
    (cond 
      (>= sub current-bar-val)
        (position-add 
          (bar-inc (set-sub g 0)) 
          (- sub current-bar-val))
      (neg? sub)
        (position-add 
          (bar-dec (set-sub g (-> g previous-bar :val))) 
          (+ sub (-> g :position :sub)))
      :else  
        (set-sub g sub))))