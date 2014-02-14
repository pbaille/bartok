(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use utils.utils))

(defn- chord-drops [[x y z & others :as coll] max-step max-size] 
  (let [octaved-up (+ y 12)]
    (if (and (<= (- z x) max-step) 
             (<= octaved-up max-size)
             (not (in? others octaved-up))
             (<= (- octaved-up (last coll)) max-step))
      (let [ret (sort (flatten (remove nil? (vector x z others octaved-up))))]
        (cons ret (chord-drops ret max-step max-size)))
      (list))))

(defn- expand-chord [coll max-step max-size] 
  (reduce 
    (fn [acc i]
      (into acc 
      (mapcat #(let [[seq1 seq2] (split-at i %)] 
                 (map (p concat seq1) 
                      (chord-drops seq2 max-step max-size))) 
              acc)))
    (cons (seq coll) (chord-drops coll max-step max-size))
    (range 1 (- (count coll) 3))))

(defn- occ-map->seq 
  "(occ-map->seq {:M2 2 :m3 1 :M6 1 :M7 1}) 
   => (2 2 3 9 11)"
  [om] 
  (sort (repeater (map vector (vals om) (map :val (b> (keys om)))))))

(defn- all-distinct? [coll]
  (if (seq coll) (a distinct? coll) true))

;;;;;;;;;;;;;;; public ;;;;;;;;;;;;;;;;

(defn all-drops [occ-map max-step max-size]
  (filter all-distinct? 
    (expand-chord (occ-map->seq occ-map) 
                  max-step 
                  max-size)))

;;;;;;;;;;;;;; examples ;;;;;;;;;;;;;;;

(use 'bartok.midi.midi)

(comment 
  (play-pitch-line 
    (->> (expand-chord [0 2 3 9 11] 9 36)
         (map (p map #(pitch (+ 48 %))))
         shuffle
         first)))

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 9 36)
         (map (p map #(pitch (+ 48 %))))
         shuffle
         first)))

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :+5 2 :+4 2 :M3 2 :M7 2} 9 36)
         (map (p map #(pitch (+ 44 %))))
         shuffle
         first)))