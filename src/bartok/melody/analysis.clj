(ns bartok.melody.analysis
  (:use midje.sweet)
  (:use bartok.types.note)
  (:use bartok.structure)
  (:use bartok.rythmn.rval)
  (:require [clojure.set :refer [subset?]] )
  (:use bartok.primitives)
  (:use utils.all))

(defn extract-pitch-classes [notes] 
  (reduce conj #{} (map (f> :pitch :pitch-class :name) notes)))

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




