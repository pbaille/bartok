(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use utils.utils))


(def ^:private b9s 
  (range 13 127 12)); => (13 25 37 49 61 73 85 97 109 121)

(defn- b9-free? 
  "return true if chord has no b9(+ n oct) in its inner voices
  means that b9 like intervals are allowed from Bass"
  [[bass frst & nxt :as chord]]
  (if-not (seq nxt)
    true
    (let [[x :as dists] (map #(- % frst) nxt)
          res (if (= 1 x) 
                true ; if first dist = 1 b9 is allowed (bill evans trick) 
                ;if-not check if dists contains no b9 
                (not (some (set dists) b9s)))] 
      (and res (b9-free? (next chord))))))

(defn- valid-drop? [d max-step max-size]
  (all-true? d
    all-distinct?
    b9-free?
    #(every? (p >= max-step) (steps %))
    #(let [sorted (sort %) fi (first sorted) la (last sorted)]
       (<= (- la fi) max-size))))

(defn- drop2s 
  "return a seq of all possible drop2 of a chord, recursive!
   while respecting max-step and max size
   ex: (drop2s [1 3 7 9] 10 24) => ((1 7 9 15) (1 9 15 19))"
  [[x y z & others :as coll] max-step max-size] 
  (let [octaved-up 
        (loop [oc (+ y 12)] 
          (if (in? others oc) (recur (+ 12 oc)) oc))]
    (if (and (<= (- z x) max-step) (<= octaved-up  max-size))
      ; create next drop... lame
      (let [ret (sort (flatten (remove nil? (vector x z others octaved-up))))]
        ; if drop is valid then keep it else compute next step with it
        (if (valid-drop? ret max-step max-size) 
          (cons ret (drop2s ret max-step max-size))
          (drop2s ret max-step max-size)))
      (list))))

(defn- drops 
  "use drop2s on the whole chord first then on (next chord) etc..."
  [coll max-step max-size] 
  (reduce 
    (fn [acc i]
      (into acc 
      (mapcat #(let [[seq1 seq2] (split-at i %)] 
                 (map (p concat seq1) 
                      (drop2s seq2 max-step max-size))) 
              acc)))
    (if (valid-drop? coll max-step max-size) 
      (cons (seq coll) (drop2s coll max-step max-size))
      (drop2s coll max-step max-size))
    (range 1 (- (count coll) 2))))

(defn- occ-map->seq 
  "(occ-map->seq {:M2 2 :m3 1 :M6 1 :M7 1}) 
   => (2 2 3 9 11)"
  [om] 
  (sort 
    (mapcat 
      (fn [[k v]]
        (for [i (range v)] 
          (+ (* i 12) (:val (b> k))))) 
      om)))

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
  (filter #(and-> % all-distinct? b9-free?) 
    (drops (occ-map->seq occ-map) max-step max-size)))

;;;;;;;;;;;;;; examples ;;;;;;;;;;;;;;;

(use 'bartok.midi.midi)

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 9 48)
         shuffle 
         first
         (map #(pitch (+ 36 %))))))

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :+5 2 :+4 2 :M3 2 :M7 2} 9 48)
         shuffle 
         first
         (map #(pitch (+ 36 %))))))



