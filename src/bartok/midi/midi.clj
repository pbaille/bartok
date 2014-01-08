(ns bartok.midi.midi
  (:use [midi])
  (:use [utils.utils])
  (:use bartok.structure.position)
  (:use [overtone.at-at]))

(def pool (mk-pool))



(defn play-note

  ([out pitch] (play-note out pitch 100 1000 0))
  ([out pitch vel](play-note out pitch vel 1000 0))
  ([out pitch vel dur](play-note out pitch vel dur 0))

  ([out pitch vel dur at]
   (after at #(midi-note-on out pitch vel) pool)
   (after (+ at dur) #(midi-note-off out pitch) pool)))

(defn play-line
  [out & pith-vel-dur-triples]
  (loop [notes (seq pith-vel-dur-triples)
         at 0] 
    (apply (partial play-note out) (conj (first notes) at))
    (if-let
      [nexts (next notes)]
      (recur nexts (+ at (last (first notes)))))))

(defn play-new [out notes]
  (let [notes (sort-by #(-> % :position position-val) notes)
        quads (map #(vector (-> % :pitch :val) 
                            (or (:velocity %)(rand-int-between 60 80)) 
                            (note-to-ms %) 
                            (pos-to-ms (:position %))) 
                   notes)]
    (for [q quads] (apply (partial play-note out) q))))

(defn lazy-drunk
  [range-bounds max-step start]
  (let [r (- (second range-bounds) (first range-bounds))
        steps (range (- max-step) (inc max-step))]
    (iterate
      (fn [l]
        (let [last-note (first l)
              available-steps (filter #(between (+ last-note %) range-bounds) steps)
              last-note (+ last-note (rand-nth available-steps))]
          (conj l last-note)))
      (seq [start]))))

(defn make-drunk-line [range-bounds max-step start length]
  (nth (lazy-drunk range-bounds max-step start) length))

(make-drunk-line [30 70] 10 35 60)

(defn make-note
  [pitch]
   [pitch 100 250])

;(def line (map make-note (lazy-drunk [50 80] 7 50 60)))
;line







