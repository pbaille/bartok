(in-ns 'bartok.core)

;ravel ma mere l'oye
(def rmmlo 
  {:1 [90 87 85 87 82 90 85 87 87 90 87 85 87 82 87 90 85 87 90 87 
       85 87 82 85 80 82 78 80 75 80 82 85 87 80 82 75 78 80 82 85 
       78 80 75 75 78 80 82 75 80 78 78 90 87 85 87 82 90 85 87 87 
       90 87 85 87 82 87 90 85 87 90 87 85 87 82 85 80 82 78 80 78 
       73 75 70 75 78 80 82], 
   :2 [61 63 68 70 66 68 75 73 61 63 70 68 66 68 73 75 61 68 70 63
       66 68 75 73 70 73 75 61 68 70 63 66 73 68 75 70 73 75 61 68
       70 63 66 73 75 68 70 75 73 61 63 70 68 66 73 75 68 70 75 73 
       61 63 70 68 66 68 75 73 70 75 73 61 71 68 66 63 61 71 68 66 
       63 61 71 68 66 63 61 70 68 63 66 75 73 68 70 73 75 61 70 68 
       63 66 73 75 68 70 73 75 61 70 68 63 66 75 73 68 70 73 61 63 
       56 66 63 61]})

(defn mc1 [len]
  (let [m-gen (->> (:1 rmmlo)
                   (markov-depth-analysis  3 [1 2 3])
                   (markov-gen))]
    (->> (take len (m-gen 90))
         (map pitch)
         (m-note-line-from (g-pos 0 0 0) 1/4 60 1))))

(defn mc2 [len]
  (let [m-gen (->> (:2 rmmlo)
                   (markov-depth-analysis  3 [1 2 3])
                   (markov-gen))]
    (->> (take len (m-gen 61))
         (map pitch)
         (m-note-line-from (g-pos 0 0 0) 1/4 60 1))))

; ; (def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))
(grid {:tempo 120 :bars [[25 :4|4]]})
(defn mc [] (play @*midi-out* (concat (mc1 100) (map #(update-in % [:pitch] transpose (c-interval :P5-d1)) (mc1 100)))))