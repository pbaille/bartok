(ns bartok.xml.transform
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.types.note)
  (:use bartok.melody.all)
  (:use bartok.structure.position)
  (:use bartok.xml.parser))

(def test-xml (parse-xml "music-files/xml/inv1.xml"))
(def msr1 (get test-xml 1)) 

(defn convert-notes-and-chords [part]
  (map-vals 
    (fn [measure]
      (update-in measure [:voices]
        (p map-vals
          (fn [voice] 
            (map (fn [nte] 
                   (if (vector? nte)
                      (chord (mapv :pitch nte) (:duration (last nte))(:position (last nte)))
                      (if (not= (:pitch nte) :none)
                        (note (:pitch nte)(:duration nte)(:position nte))
                        (r-note (:duration nte) (:position nte)))))
            voice)))))
    part))

(defn expand-attributes [part]
  (map-reduce #(update-in %2 [:attributes] if-nil-merge (:attributes %1)) 
              (get (vals part) 0)
              (vals part)))

(defn to-grid-bars [part]
  (->> (expand-attributes part)
       (map (f> :attributes :time :name))
       (partition-by identity)
       (map #(vector (count %) (first %)))))

;(->> test-xml to-grid-bars)
;=> ([22 :4|4])

(defn measure-notes [m] 
  (->> m :voices vals (a concat) flatten 
      (map #(note (:pitch %)(:duration %)(:position %)))))

(defn extract-voice [parsed n]
  (->> (vals parsed)
       (map (f> :voices (get n)))))

(defn voice->steps [voice]
  (let [just-pitches (map #(if (vector? %) (map :pitch %) (:pitch %)) voice)
        pitch-line (map #(if (sequential? %) (:name (a highest %)) %) just-pitches)]
    (for [[p1 p2] (partition 2 1 pitch-line)] (c-interval p1 p2))))



;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;

(def qsd (convert-notes-and-chords test-xml))
(def v1 (extract-voice qsd 1))