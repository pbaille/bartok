(ns bartok.harmony.chords
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use utils.all))

(def ^:private 
  lower-interval-limits
  {:m2 :E-1
   :M2 :Eb-1
   :m3 :C-1
   :M3 :Bb-2
   :P4 :Bb-2
   :+4 :Bb-2
   :P5 :Bb-3
   :+5 :G-2
   :M6 :F-2
   :m7 :F-2
   :M7 :F-2})

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
    b9-free?
    #(every? (p >= max-step) (steps %))
    #(<= (last %) max-size)))

(defn- drops 
  "awesome docstring"
  ([coll max-step max-size] 
    (if (valid-drop? coll max-step max-size)
      ;coll is valid drop so append it to the results
      (cons (seq coll) (drops coll 0 max-step max-size))
      ;coll is not valid so continue without keeping it
      (drops coll 0 max-step max-size)))
  ([coll idx max-step max-size] 
  (let [[head [x y z & others]] (split-at idx coll)
        octaved-up 
        ;find first possible 'octavation'
        (loop [oc (+ y 12)] 
          (if (in? others oc) (recur (+ 12 oc)) oc))]
    (if-not z 
      (list) ;no possible continuation
      ;else continue
      (if (and (<= (- z x) max-step) (<= octaved-up  max-size))
        ; drop is allowed so build it
        (let [ret (concat head (->> (vector x z others octaved-up) (remove nil?) flatten sort))]
          (if (valid-drop? ret max-step max-size) 
            ;drop is valid so keep it and continue
            (cons ret (concat (drops ret idx max-step max-size) 
                              (drops coll (inc idx) max-step max-size)))
            ;drop isn't valid so continue without keeping it
            (concat (drops ret idx max-step max-size) 
                    (drops coll (inc idx) max-step max-size))))
        ;drop is not allowed so recur with (inc idx)
        (drops coll (inc idx) max-step max-size))))))

(defn- occ-map->seq 
  "(occ-map->seq {:M2 2 :m3 1 :M6 2 :M7 1}) 
   => (2 3 9 11 14 21)"
  [om] 
  (sort 
    (mapcat 
      (fn [[k v]]
        (for [i (range v)] 
          (+ (* i 12) (:val (b> k))))) 
      om)))

(defn- chord-inversions 
  "takes a drop and returns its inversions
  ex:
  (chord-inversions [0 2 3 9 14])
  => [[0 2 3 9 14] [2 3 9 12 14] [3 9 12 14 26] [9 12 14 15 26]]"
  [d]
  (iterate-while
    #(<= (second (last %)) 12) 
    (fn [acc]
      (let [[fl & others :as l] (last acc)
            oc (loop [x (+ fl 12)] 
                 (if (in? others x) (recur (+ 12 x)) x))]
        (conj acc (vec (sort (conj others oc))))))
    [d]))

;;;;;;;;;;;;;;; public ;;;;;;;;;;;;;;;;

(defn all-drops 
  "compute all possible drops of a chord
  args:
  occ-map {c-int-class occurence ...}
  max-step (the max interval in semitone between to adjacent notes of a chord)
  max-size (maximum total size of the drops)
  ex:
  (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 9 36)"
  ([occ-map] (all-drops occ-map {}))
  ([occ-map 
    {:keys [max-step max-size inversions] 
     :or {max-step 9 max-size 36 inversions false}}]
    (if inversions 
      (let [invs (chord-inversions (occ-map->seq occ-map))]
        (map (fn [inv] 
               ; if only drop2s works well there will be no need to refilter results...
               (filter #(valid-drop? % max-step (+ max-size (first inv))) 
                       (drops inv max-step (+ max-size (first inv))))) 
             invs))
      (filter #(valid-drop? % max-step max-size) 
              (drops (occ-map->seq occ-map) max-step max-size)))))

(defn drops-from 
  "docstring"
  ([root-pitch occ-map] 
    (drops-from root-pitch occ-map {}))
  ([root-pitch occ-map {invs :inversions :as options}]
    (let [c-int-map (zipmap (map (f> b> :val)(keys occ-map))
                            (keys occ-map))
          drops (if invs 
                  (a concat (all-drops occ-map options))
                  (all-drops occ-map options))]
      (map (p map 
              #(transpose 
                 root-pitch 
                 (c-interval (c-int-map (mod12 %)) 
                             (int-div % 12)))) 
           drops)))
  ([bass top occ-map options]
     (let [bass (b> bass) top (b> top)
           max-size (- (:val top) (:val bass))]
       (filter #(and (= bass (first %))(= top (last %))) 
             (drops-from bass occ-map (assoc options :max-size max-size))))))

(b-multi voicing)
(b-meth voicing 
  ['Pitch 'Pitch 'Mode :number] [bass top mod n-voices]
  ())

;;;;;;;;;;;;;; examples ;;;;;;;;;;;;;;;


(comment 
  (time (count (drops [0 2 3 9 11 14 15 21 23] 11 48)))
  (all-distinct? (drops [0 2 3 9 11 14 15 21 23] 11 48)))

(use 'bartok.midi.midi)

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :M6 2 :+4 2 :M3 2 :M7 2} 
                    {:max-step 7 
                     :max-size 48})
         shuffle 
         first
         (map #(pitch (+ 36 %))))))

(comment 
  (play-pitch-line 
    (->> (all-drops {:P1 2 :+5 2 :+4 2 :M3 2 :M7 2})
         shuffle 
         first
         (map #(pitch (+ 32 %))))))

(comment 
  (play-chord
    (->> (drops-from :C-1 :F#0 {:P1 1 :M6 1 :+4 1 :M3 1 :#2 1 :M7 1} {})
         shuffle 
         first)))

(comment 
  (play-chord
    (->> (drops-from :Bb-2 :G0 {:P1 1 :+4 1 :M6 1 :m7 1 :M2 1} {})
         shuffle 
         first)))

(comment 
  (map (p map :name) 
       (drops-from :Bb-2 :Eb1 {:P1 1 :P5 1 :P4 1 :M6 1 :m7 1 :M2 1} {})))



