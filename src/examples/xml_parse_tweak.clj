(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def score (parse-xml "music-files/xml/jedall.xml"))

(def d-ints (voice->d-int (take 200 (second (process-score score)))))
(def contour (voice->contour (take 200 (second (process-score score)))))
(def clyd (melodic-domain :C-Lyd [:C-2 :C3] :C0))

(grid-assoc :tempo 120)

(defn rav-steps 
  "map 'jeux d'eau' steps on alternate C-Lyd and Ab-Lyd modes"
  [len]
  (as-> (take len d-ints) x
        (map (fn [steps mod] 
              {:mode mod :steps steps}) 
             (partition 16 16 nil x) (cycle [:C-Lyd :Ab-Lyd]))
        (step-sequence x [:C-4 :C4] :C0)
        ; (dr)
        (m-note-line-from (g-pos 0 0 0) 1/4 60 1 x)))

;constrained markov generator with accumulator
(def cmgwa (->> (take 150 d-ints)
                (markov-depth-analysis 3 4)
                c-markov-gen-with-acc))

(defn rav-step-line 
  "return a m-note sequence based on 'jeux d'eau' top voice steps"
  [len]
  (->> (take len (cmgwa 
                  ;constraints
                  (fn [mel-dom chain-so-far stp]
                    (and 
                      ;check if stp is a possible step on mel-dom
                      (step mel-dom stp)
                      ;remove repetitions (step 0)
                      (not= 0 (to-num stp))))
                  ;init mel-domain
                  clyd
                  ;called to update domain at each step
                  (fn [acc nxt] (step acc nxt))))
       ;construct a step sequence based on returned chain
       (step-sequence clyd)
       ;to midi-notes
       (m-note-line-from (g-pos 0 0 0) 1/4 60 1)))

(defn ctt []
  (let [ints-vals (map to-num d-ints)
        fpm (zipmap ints-vals (repeat 1))
        transcntr (flatten (:results (contour-prob-line (take 50 ints-vals) fpm clyd 3)))
        bnds (steps-bounds transcntr)
        diff (- (:val (first (interval-bounds clyd))) (first bnds))
        ss (step-sequence (step clyd diff) transcntr)
        mn-line (m-note-line-from (g-pos 0 0 0) 1/2 60 1 ss)]
  (dr)
  (grid {:bars [[24 :4|4]] :tempo 120})
  (play @*midi-out* mn-line)
  ))