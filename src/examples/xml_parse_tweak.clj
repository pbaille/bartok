(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def score (parse-xml "music-files/xml/jedall.xml"))

(def d-ints (voice->d-int (take 200 (second (process-score score)))))
(def clyd (melodic-domain :C-Lyd [:C-2 :C3] :C0))

(grid-assoc :tempo 120)

(defn lydeau [md len]
  (->> (remove #(> (abs (:val %)) 8) d-ints)
       (map :name)
       (markov-depth-analysis  6 2)
       (constraint-markov-chain 
         (fn [acc [k v]]
           (and 
             ;constraints (have to make a macro...)
             (steps-in-bounds? md (conj acc k))
             (let [[mn mx] (steps-bounds (map to-num (take-last 3 (conj acc k))))]
               (and (> mn -12) (< mx 12)))
             (not= k :1st-u)))
         len 
         :4th-u)
       (step-sequence md)
       (m-note-line-from (g-pos 0 0 0) 1/4 60 1)))

(defn rav-steps [md len]
  (as-> (take len d-ints) x
        (map (fn [steps mod] 
              {:mode mod :steps steps}) 
             (partition 16 16 x) (cycle [:C-Lyd :Ab-Lyd]))
        (step-sequence x [:C-4 :C4] :C0)
        ; (dr)
        (m-note-line-from (g-pos 0 0 0) 1/4 60 1 x)))

;;;;;;;;;;;;;;;; test new markov funs ;;;;;;;;;;;;;;

(def c-laz (->> (take 50 d-ints)
                (markov-depth-analysis 6 4)
                c-markov-gen))

(defn rav-step-line [len]
(->> (take len (c-laz 
                ;check if stp is a possible step on mel-dom
                (fn [mel-dom chain-so-far [stp _]]
                  (step mel-dom stp))
                ;init mel-domain
                (melodic-domain :C-Lyd [:C-2 :C2] :C0)
                ;called to update domain at each step
                (fn [acc nxt] (step acc nxt))))
     ;construct a step sequence based on returned chain
     (step-sequence (melodic-domain :C-Lyd [:C-1 :C2] :C0))
     ;to midi-notes
     (m-note-line-from (g-pos 0 0 0) 1/4 60 1)))
