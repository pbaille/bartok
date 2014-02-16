(ns bartok.midi.midi
  (:use [bartok.midi.overtone-midi])
  (:use [utils utils macros])
  (:use bartok.structure)
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
   (after (+ at (- dur 0.00001)) #(midi-note-off out pitch chan) pool)
   nil))

;expand chords into notes
(defn expand-chords [notes&chords]
  (mapcat (fn [el]
            (cond 
              (type= el 'Note) (vector el)
              (type= el 'Chord) 
                (let [{:keys [duration position velocity channel pitches] :or {velocity 60 channel 0}} el
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
  (m-note (:val pitch) (float (note-to-ms note)) (float (pos-to-ms position)) velocity channel))

(defn play [out notes]
  (let [notes_ (map to-midi (expand-chords notes))]
    (doseq [{p :pitch v :velocity d :duration pos :position c :channel} notes_]
      (play-note out p v d pos c))))

(use 'bartok.composition.utils)
(use 'bartok.state)

(defn play-pitch-line 
  "little helper to save keystrokes: 
  play all pitches in a row with equal duration vel and chan
  ex: (play-pitch-line [:C0 :D0 :E0])"
  ([pitches] (play-pitch-line pitches {}))
  ([pitches
   {:keys [velocity duration channel tempo]
    :or
    {duration 1/4
     velocity 60
     channel  1
     tempo    120}}]
  (grid {:bars [[32 :4|4]] :tempo tempo})
  (->> pitches 
      (m-note-line-from (g-pos) duration velocity channel)
      (play @*midi-out*))))

(defn play-chord 
  ([pitches] (play-chord pitches 4))
  ([pitches dur]
   (grid {:bars [[32 :4|4]] :tempo 120})
   (play @*midi-out* [(chord pitches dur (g-pos))])))


