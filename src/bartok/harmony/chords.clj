(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use utils.utils))

(defn- drop2s 
  "return a seq of all possible drop2 of a chord, recursive!
   while respecting max-step and max size
   ex: (drop2s [1 3 7 9] 10 24) => ((1 7 9 15) (1 9 15 19))"
  [[x y z & others :as coll] max-step max-size] 
  (let [octaved-up (+ y 12)]
    (if (and (<= (- z x) max-step) 
             (<= octaved-up max-size)
             (not (in? others octaved-up))
             (<= (- octaved-up (last coll)) max-step))
      (let [ret (sort (flatten (remove nil? (vector x z others octaved-up))))]
        (cons ret (drop2s ret max-step max-size)))
      (list))))

(defn- drops 
  "use drop2s on the whole chord first then on butfirst chord etc..."
  [coll max-step max-size] 
  (reduce 
    (fn [acc i]
      (into acc 
      (mapcat #(let [[seq1 seq2] (split-at i %)] 
                 (map (p concat seq1) 
                      (drop2s seq2 max-step max-size))) 
              acc)))
    (cons (seq coll) (drop2s coll max-step max-size))
    (range 1 (- (count coll) 3))))

(defn- occ-map->seq 
  "(occ-map->seq {:M2 2 :m3 1 :M6 1 :M7 1}) 
   => (2 2 3 9 11)"
  [om] 
  (sort (repeater (map vector (vals om) (map :val (b> (keys om)))))))

(defn- all-distinct? 
  "true if coll contains only distinct values"
  [coll]
  (if (seq coll) (a distinct? coll) true))

;;;;;;;;;;;;;;; public ;;;;;;;;;;;;;;;;

(defn all-drops 
  "compute all possible drops of a chord
  args:
  occ-map {c-int-class occurence ...}
  max-step (the max interval in semitone between to adjacent notes of a chord)
  max-size (maximum total size of the drops)
  ex:
  (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 9 36)"
  [occ-map max-step max-size]
  (filter all-distinct? 
    (drops (occ-map->seq occ-map) max-step max-size)))

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