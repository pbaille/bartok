(ns bartok.xml.transform
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.types.note)
  (:use bartok.melody.all)
  (:use bartok.structure)
  (:use bartok.xml.parser))


;;;helpers for mapping part notes
(defn map-notes 
  "helper to map notes of a part
  takes a fun that takes 3 args:
  [measure_number measure] [voice_number voice] note
  and apply it on each note"
  [fun part]
  (map-h
    (fn [mn measure]
      {mn (update-in measure [:voices]
             (p map-h
               (fn [vn voice]
                 {vn (map (p fun [mn measure] [vn voice]) voice)})))})
  part))

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

(defn bar-count 
  "return the number of bars in the score"
  [score]
  (best > (map count score)))

(defn measure-notes 
  "return all notes (from all voices) of a measure"
  [m] 
  (->> m :voices vals (a concat) flatten 
      (map #(note (:pitch %)(:duration %)(:position %)))))

(defn extract-voice 
  "extract voice n from a part
   return seq of notes vecs (one vec per measure)"
  [part n]
  (->> (vals part)
       (map (f> :voices (get n)))))

(defn extract-voices 
  "extract voices from a part
   return seq of voices (notes vecs)"
  [part] 
  (reduce (fn [acc measure]
            (reduce #(into %1 %2) 
                    acc 
                    (vals measure))) 
          [] 
          (map :voices (vals part))))

(defn voice->steps 
  "convert a voice into seq of c-intervals"
  [voice]
  (let [just-pitches (map #(if (vector? %) (map :pitch %) (:pitch %)) voice)
        pitch-line (map #(if (sequential? %) (:name (a highest %)) %) just-pitches)]
    (for [[p1 p2] (partition 2 1 pitch-line)] (c-interval p1 p2))))

;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;

; for grid read/write
(use 'bartok.state)
(use 'bartok.midi.overtone-midi)
(use 'bartok.midi.midi)

(def score (parse-mxl "music-files/mxl/Promenade for Brass Quintet.mxl"))

(defn attrs-tm>>*g* 
  "feed *g* atom with score structure"
  [score]
  (let [atm (attrs-tm (first score))
        bars (reduce #(conj %1 (atm %2 :time)) 
                     [] (range 0 (bar-count score)))
        tempo (map vec (:tempo (atm)))]
    (grid {:bars bars :tempo tempo})))

(defn add-grid-pos 
  "add bar offset to each note position of part"
  [part]
  (map-notes (fn [[nm m][nv v]n]
               (if (vector? n)
                 (mapv #(assoc % :position 
                         (num->pos (+ (:position %) 
                                      (pos-val (g-pos {:bar nm})))))
                      n)
                 (assoc n :position 
                   (num->pos (+ (:position n) 
                                (pos-val (g-pos {:bar nm}))))))) 
             part))

(defn concert-pitch 
  "transpose part to concert pitch if transposed"
  [part]
  (let [tr (-> part (get 0) :attributes :transpose)]
    (map-notes (fn [m v nte] 
                 (if (and tr (not= (:pitch nte) :none))
                   (assoc nte :pitch (transpose (:pitch nte) tr))
                   nte))
               part)))

(defn convert-notes-and-chords 
  "convert notes to bartok objects (note, chord, r-note)"
  [part]
  (map-notes
    (fn [[mn m][vn v] nte] 
      (if (vector? nte)
         (chord (mapv :pitch nte) (:duration (last nte))(:position (last nte)))
         (if-not (= (:pitch nte) :none)
           (note (:pitch nte)(:duration nte)(:position nte))
           (r-note (:duration nte) (:position nte))))) 
    part))

(defn add-midi-chans 
  "add midi channel to every notes (one distinct by part)"
  [score]
  (map-indexed 
    (fn [part-num part] 
      (map-notes #(assoc %3 :channel part-num) 
                 part))
    score))

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn play-score [score]
  (attrs-tm>>*g* score) ;feed grid  
  (->> score
       (map (c convert-notes-and-chords 
               concert-pitch
               add-grid-pos))
       add-midi-chans
       (map extract-voices)
       (a concat)
       ; (dr)
       (play vep)
       ))

