(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def score (parse-xml "music-files/xml/jedall.xml"))

(def d-ints (voice->d-int (take 200 (second (process-score score)))))
(def clyd (melodic-domain :C-Lyd [:C-2 :C3] :C0))

(grid-assoc :tempo 120)

(defn rav-steps 
  "map 'jeux d'eau' steps on alternate C-Lyd and Ab-Lyd modes"
  [md len]
  (as-> (take len d-ints) x
        (map (fn [steps mod] 
              {:mode mod :steps steps}) 
             (partition 16 16 x) (cycle [:C-Lyd :Ab-Lyd]))
        (step-sequence x [:C-4 :C4] :C0)
        ; (dr)
        (m-note-line-from (g-pos 0 0 0) 1/4 60 1 x)))

;;;;;;;;;;;;;;;; test new markov funs ;;;;;;;;;;;;;;

;constrained markov generator with accumulator
(def cmgwa (->> (take 150 d-ints)
                (markov-depth-analysis 3 4)
                c-markov-gen-with-acc))

(defn rav-step-line [len]
(->> (take len (cmgwa 
                ;check if stp is a possible step on mel-dom
                (fn [mel-dom chain-so-far stp]
                  (step mel-dom stp))
                ;init mel-domain
                clyd
                ;called to update domain at each step
                (fn [acc nxt] (step acc nxt))))
     ;construct a step sequence based on returned chain
     (step-sequence clyd)
     ;to midi-notes
     (m-note-line-from (g-pos 0 0 0) 1/4 60 1)))
