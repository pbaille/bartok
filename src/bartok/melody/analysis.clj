(ns bartok.melody.analysis
  (:use midje.sweet)
  (:require [bartok.midi.xml-parser :as xp]) ;;;;;for tests
  (:use bartok.types.note)
  (:use bartok.structure.position)
  (:use bartok.rythmn.rval)
  (:use bartok.composition.utils)
  (:require [clojure.set :refer [subset?]] )
  (:use bartok.primitives)
  (require [utils.all :refer :all]))

(defn ginf [& keys] #(get-in % keys))

(defn- extract-pitch-classes [notes] (reduce conj #{} (map (f> :pitch :pitch-class :name) notes)))

(defn find-mothers [notes]
  (let [pcns (extract-pitch-classes notes)
        res (filter
              #(subset? pcns (->> % :pitch-classes (map :name) set))
              mothers)]
    (if (seq res) (map :name res) nil)))

; (defn closest-mothers [notes]
;   (let [by-den (map->sorted (group-by (f> :position :sub denom) notes))]
;     ; (pp (map-vals #(map (f> :pitch :name) %) by-den))
;     (last (take-while seq (for [c (range 1 (inc (count by-den)))] 
;                             (->> (take c by-den) (map second) (a concat) find-mothers pev))))))

(defn closest-mothers [notes]
  (let [nts (map (asf>  (assoc _ :val (/ (-> _ :position :sub denom (* 2)))
                                 :name (-> _ :pitch :pitch-class :name))
                        (dissoc :duration :position :pitch)) 
                 notes)
        modes (reduce #(assoc %1 
                         (:name %2) 
                         {:pcs (->> %2 :pitch-classes (map :name)) 
                         :val 0}) 
                      {} mothers)]
    (->> (reduce (fn [acc [{nam :name va :val} [mn {pcs :pcs}]]]
                   (if (in? pcs nam)
                     (if (mn acc) 
                       (update-in acc [mn] + va) 
                       (assoc acc mn va))
                     acc)) 
                 {} 
                 (for [n nts m modes] [n m]))
         (sort-by val)
         reverse)))

(defn closest-mothers2 [notes]
  (let [nts (map (asf> (assoc _ :val (/ (-> _ :position :sub denom (* 2)))
                                :name (-> _ :pitch :pitch-class :name))
                       (dissoc :duration :position :pitch)) 
                 notes)
        *modat* (atom (reduce #(assoc %1 
                                 (:name %2) 
                                 {:pcs (->> %2 :pitch-classes (map :name)) 
                                 :val 0}) 
                              {} mothers))]
    (doseq [{nam :name va :val} nts [mn {pcs :pcs}] @*modat*] 
      (when (in? pcs nam)
        (swap! *modat* #(update-in % [mn :val] + va))))
     (reverse (sort-by (f> val :val) @*modat*))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(grid)
(def- notes (note-line-from (g-pos 0 0 0) 1 :C0 :D0 :Ab0))
(def- notes2 (note-line-from (g-pos 0 0 0) 1/4 :C0 :D0 :Eb0 :G0 :Ab0 :Bb0 :C0 :D0 :C#0 :D0 :C0 :D0 :Eb0 :Ab0 :Bb0 :C0 :D0 :C#0 :D0 :C0 :D0 :Eb0 :Ab0 :Bb0 :C0 :D0 :C#0 :D0))

(fact "ginf"
  ((ginf :a :aa) {:a {:aa 1}}) => 1)

(fact "extract pitch classes"
  (extract-pitch-classes notes) => #{:C :D :Ab})

(fact "find-mothers"
  (find-mothers notes) => [:Ab-Lyd :Ab-Lyd+ :Gb-Lyd+ :Ab-Lyd#2])


;;;;;;;;;;;;;;;;;;;;; analysing parsed xml ;;;;;;;;;;;;;;;;;;;;;;;;;;

(def score (xp/main "src/music-files/xml/noct21.xml"))
(def- msr1 (get score 1))

(defn closest-mothers3 [notes]
  (let [nts (map (asf>  (assoc _ :val (/ (-> _ :position :sub denom (* 2)))
                                 :name (-> _ :pitch :pitch-class :name))
                        (dissoc :duration :position :pitch)) 
                 notes)
        modes (reduce #(assoc %1 
                         (:name %2) 
                         {:pcs (->> %2 :pitch-classes (map :name)) 
                         :val 0}) 
                      {} mothers)
        results (reduce #(assoc %1 
                            (:name %2)
                            {:match-points 0 
                             :in [] 
                             :out []}) 
                         {} mothers)]
    (->> (reduce (fn [acc [{nam :name va :val} [mn {pcs :pcs}]]]
                   ; (pp (in? pcs nam))
                   (if (in? pcs nam)
                      (-> acc 
                          (update-in [mn :match-points] + va)
                          (update-in [mn :in] conj nam)) 
                      (update-in acc [mn :out] conj nam))) 
                 results 
                 (for [n nts m modes] [n m]))
         (sort-by #(:match-points (second %)))
         reverse)))

(defn- measure-notes [m] 
  (->> m :voices vals (a concat) flatten 
       (map #(note (:pitch %)(:duration %)(:position %)))))

(defn- harmonic-an [m]
  (closest-mothers3 (measure-notes m)))

(defn- voice->steps [voice]
  (let [just-pitches (map #(if (vector? %) (map :pitch %) (:pitch %)) voice)
        pitch-line (map #(if (sequential? %) (:name (a highest %)) %) just-pitches)]
    (for [[p1 p2] (partition 2 1 pitch-line)] (do (dr)(c-interval p1 p2)))))

