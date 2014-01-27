(ns bartok.xml.transform
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.types.note)
  (:use bartok.melody.all)
  (:use bartok.structure)
  (:use bartok.xml.parser))


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

;;;exp;;;
; (defmacro map-notes [fun score]
;     `(map-vals 
;       (fn [measure#]
;         (update-in measure [:voices]
;           (p map-vals
;             (fn [voice#] 
;               (map (p fun measure# voice#)
;               voice)))))
;     ~score))

(def score (parse-mxl "music-files/mxl/Promenade for Brass Quintet.mxl"))
; (map-notes (fn [measure voice nte] 
;               "yo")
;            score)

(defn expand-attributes [part]
  (map-reduce #(update-in %2 [:attributes] if-nil-merge (:attributes %1)) 
              (get (vals part) 0)
              (vals part)))

(defn expand-tempo [part]
  (map-reduce (fn [acc el]
                (if-not (:tempo el) 
                 (assoc el :tempo 
                   (or (:tempo acc)
                       {:bpm (get-in acc [:directions :tempo]) 
                        :beat-unit :quarter})))) 
              (first (vals part))
              (vals part)))

(defn to-grid-bars [part]
  (->> (expand-attributes part)
       (map (f> :attributes :time :name))
       (partition-by identity)
       (mapv #(vector (count %) (first %)))))

; (defn to-grid-tempi [part]
;   ())

(defn bar-count 
  "return the number of bars in the score"
  [score]
  (best > (map count score)))

(defn attrs-tm 
  "takes a part (from parsed xml score)
  return a function that return attrs at a given time (see bartok.structure/time-map)
  (def aze (attrs-tm (first (parse-mxl \"mxl file path\"))))
  (aze 10) => return attrs at measure 10 of the score"
  [part]
  (reduce 
    (fn [tm [k v]]
      (let [{attr :attributes dir :directions :or {attr {} dir {}}} v
             all (dissoc-nils (merge attr dir))]
        (reduce (fn [tm2 [attr value]](tm2 k attr value)) tm all))) 
    (time-map [:transpose :divisions :time :key :dynamics :tempo])
    part))

;(->> test-xml to-grid-bars)
;=> [[22 :4|4]]

(defn measure-notes 
  "return all notes (from all voices) of a measure"
  [m] 
  (->> m :voices vals (a concat) flatten 
      (map #(note (:pitch %)(:duration %)(:position %)))))

(defn extract-voice [part n]
  "extract voice n from a part
   return seq of vecs of notes (one vec per measure)"
  (->> (vals part)
       (map (f> :voices (get n)))))

(defn voice->steps 
  "convert a voice into seq of c-intervals"
  [voice]
  (let [just-pitches (map #(if (vector? %) (map :pitch %) (:pitch %)) voice)
        pitch-line (map #(if (sequential? %) (:name (a highest %)) %) just-pitches)]
    (for [[p1 p2] (partition 2 1 pitch-line)] (c-interval p1 p2))))



;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;

; (def brass-quintet (map convert-notes-and-chords 
;                         (parse-mxl "music-files/mxl/Promenade for Brass Quintet.mxl")))

; for grid read/write
(use 'bartok.state)
(use 'bartok.midi.overtone-midi)
(use 'bartok.midi.midi)

(defn attrs-tm>>*g* 
  "feed *g* atom with score structure"
  [score]
  (let [atm (attrs-tm (first score))
        bars (reduce #(conj %1 (atm %2 :time)) 
                     [] (range 0 (bar-count score)))
        tempo (map vec (:tempo (atm)))]
    (grid {:bars bars :tempo tempo})))

(defn add-grid-pos [part]
  (map-h 
    (fn [num measure]
      {num 
      (update-in measure [:voices] 
        (p map-vals 
          (fn [voices]
            (map #(assoc % :position 
                    (num->pos (+ (:position %) 
                                 (pos-val (g-pos {:bar num}))))) 
                 voices))))}) 
    part))

(defn concert-pitch [part]
  (map-h 
    (fn [num measure]
      {num 
      (update-in measure [:voices] 
        (p map-vals 
          (fn [voices]
            (map (fn [nte] (if-let [tr (-> part (get 0) :attributes :transpose)]
                    (if-not (= (:pitch nte) :none) 
                      (assoc nte :pitch (if-let [p (transpose (:pitch nte) tr)]
                                          p (dr)))
                      nte)
                    nte)) 
                 voices))))}) 
    part))

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn play-score [score]
  (attrs-tm>>*g* score) ;feed grid  
  (->> (map (c #(flatten (extract-voice % 1)) 
               convert-notes-and-chords 
               concert-pitch
               add-grid-pos) 
            score)
       ; (a concat)
       ; (play vep)
       ))

