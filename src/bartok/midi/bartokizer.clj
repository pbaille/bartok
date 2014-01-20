(ns bartok.midi.bartokizer
  (:use utils.all)
  (:use vendors.debug-repl)
  (:use [bartok.midi overtone-midi midi parser])
  (:use bartok.structure.position)
  (:use bartok.types.note)
  (:use bartok.litterals.all)
  (:use bartok.midi.parser))

(defn- total-length [tracks]
  (->> (for [t tracks] (best > (map :position t)))
       (remove nil?)
       (best >)))

(defn- to-note 
  [{pos :position p :pitch d :duration c :channel v :velocity :as n}]
   (let [pos (num->pos pos)
         m (mode-at pos)]
     (note (pitch m p) d pos v c)))

;try time-signature convert
(defn ts-convert [tsh end]
  (->> (sort (assoc tsh end nil)) 
       (partition 2 1)
       (mapcat
         (fn [[[k1 v1][k2]]]
           (let [ts (a time-signature v1)]
             (repeat (/ (- k2 k1) (:val ts)) ts))))))

(defn ks-convert [ks]
  (->> (sort-by first ks)
       (map (fn [[k v]] 
                (let [pos (num->pos k)
                      posv [(:bar pos)(:sub pos)]
                      harmo (mode (kwcat v "-Ion"))]
                  {:position posv :mode harmo})))))

(defn group-types [tracks] 
  (for [t tracks] (group-by type t)))

(defn notes-conversion [tracks]
  (for [t tracks]
    (let [{notes :note :as track} t]
      (if notes 
        (assoc track :note (map to-note notes))
        track))))

(defn feed-grid-with-meta [tracks]
  (let [total-len (total-length tracks)
        tracks (group-types tracks)]
    (dorun (for [t tracks]
      (let [{[{t :tempo ks :key-signature ts :time-signature}] 
             :meta-messages :as track} t]
        (do
          (when t  (grid-assoc :tempo (mapv #(mapv int %) (sort-by first (vec t)))))
          (when ts (grid-assoc :bars (ts-convert ts total-len)))
          (when ks (grid-assoc :harmony (ks-convert ks)))))))))

(grid)

(defn bartokize [path]
  (let [tracks (parse-midi-file path)]
    (feed-grid-with-meta tracks)  
    (->> tracks group-types notes-conversion)))

; (bartokize "src/midi-files/jeuxdeau.mid")

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))
(def score (bartokize "src/midi-files/fur_elise.mid"))


