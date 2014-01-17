(ns bartok.midi.bartokizer
  (:use utils.utils)
  (:use vendors.debug-repl)
  (:use bartok.structure.position)
  (:use bartok.types.note)
  (:use bartok.litterals.all)
  (:use bartok.midi.parser))


(defn- to-note 
  [{pos :position p :pitch d :duration c :channel v :velocity :as n}]
   (note (pitch p) d (num->pos pos) v 1))

(defn group-types [tracks] 
  (for [t tracks] (group-by type t)))

(defn notes-conversion [tracks]
  (for [t tracks]
    (let [{notes :note :as track} t]
      (if notes 
        (assoc track :note (map to-note notes))
        track))))

(defn feed-grid-with-meta [tracks]
  (for [t tracks]
    (let [{[{t :tempo ks :key-signature ts :time-signature}] 
           :meta-messages :as track} t]
      (if t
        (do (debug-repl) (grid-assoc :tempo (vec t) ))
        track))))

; (grid-assoc :tempo (mapv #(vector (:position %) (:bpm %)) 
;                           (filter-msg-type :tempo jedo)))


(grid)

(def go-test 
  (->> (parse-midi-file "src/midi-files/bumble_bee.mid")
       group-types
       notes-conversion
       feed-grid-with-meta))

;try time-signature convert
(->> {0 [2 4] 16 [3 4]}
     (map 
       #()))

