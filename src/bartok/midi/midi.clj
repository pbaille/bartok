(ns bartok.midi.midi
  (:use [bartok.midi.overtone-midi])
  (:use [utils utils macros])
  (:use bartok.structure.position)
  (:use bartok.types.note)
  (:use [overtone.at-at]))

(def pool (mk-pool))

;doesn't work
(defn stop-all []
  (stop-and-reset-pool! pool))


(defn play-note
  ([out pitch] (play-note out pitch 100 1000 0 1))
  ([out pitch vel](play-note out pitch vel 1000 0 1))
  ([out pitch vel dur](play-note out pitch vel dur 0 1))
  ([out pitch vel dur at](play-note out pitch vel dur at 1))
  ([out pitch vel dur at chan]
   (after at #(midi-note-on out pitch vel chan) pool)
   (after (+ at (- dur 0.00001)) #(midi-note-off out pitch chan) pool)))

;expand chords into notes
(defn expand-chords [notes&chords]
  (mapcat (fn [el]
            (cond 
              (type= el 'Note) (vector el)
              (type= el 'Chord) 
                (let [{:keys [duration position velocity channel pitches]} el
                      w-pitches (dissoc el :pitches)]
                  (map #(note % duration position velocity channel) pitches)))) 
          (sort-by (c pos-val :position) notes&chords)))

(defn m-note 
  [pitch duration position velocity channel]
  (with-type 'MidiNote
    {:pitch pitch
     :duration duration
     :position position
     :velocity velocity
     :channel channel}))

(defn to-midi
  [{:keys [pitch position velocity channel] 
      :or {pitch    60 
           position 0 
           velocity 60 
           channel  0}
      :as note }]
  (m-note (:val pitch) (note-to-ms note) (pos-to-ms position) velocity channel))

(defn play [out notes]
  (let [notes (map to-midi (expand-chords notes))]
    (for [{p :pitch v :velocity d :duration pos :position c :channel} notes]
      (play-note out p v d pos c))))

;************* old ***************

;(defn play [out notes]
;   (let [notes (sort-by #(-> % :position pos-val) notes)
;         quads (map #(vector (-> % :pitch :val) 
;                             (or (:velocity %)(rand-int-between 60 80)) 
;                             (note-to-ms %) 
;                             (pos-to-ms (:position %))) 
;                    notes)]
;     (for [q quads] (apply (partial play-note out) q))))



; (defn play-line
;   [out & pith-vel-dur-triples]
;   (loop [notes (seq pith-vel-dur-triples)
;          at 0] 
;     (apply (partial play-note out) (conj (first notes) at))
;     (if-let
;       [nexts (next notes)]
;       (recur nexts (+ at (last (first notes)))))))

; (defn lazy-drunk
;   [range-bounds max-step start]
;   (let [r (- (second range-bounds) (first range-bounds))
;         steps (range (- max-step) (inc max-step))]
;     (iterate
;       (fn [l]
;         (let [last-note (first l)
;               available-steps (filter #(between (+ last-note %) range-bounds) steps)
;               last-note (+ last-note (rand-nth available-steps))]
;           (conj l last-note)))
;       (seq [start]))))

; (defn make-drunk-line [range-bounds max-step start length]
;   (nth (lazy-drunk range-bounds max-step start) length))

; (make-drunk-line [30 70] 10 35 60)

; (defn make-note
;   [pitch]
;    [pitch 100 250])

;(def line (map make-note (lazy-drunk [50 80] 7 50 60)))
;line







