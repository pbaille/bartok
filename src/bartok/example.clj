(in-ns 'bartok.core)

; (def opt
;   {:tempo-humanize [1 5 2 1/2]})

(grid {:bars [[2 :4|4]] 
       :tempo [[0 2 120]] 
       :harmony {[0 0] :C-Lyd
                 [1 0] :Ab-Lyd}})

                 ; [2 0] :Eb-Lyd+
                 ; [3 0] :B-Lyd+
                 ; [4 0] :A-Lyd
                 ; [5 0] :F-Lyd
                 ; [6 0] :F-Melm
                 
(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{4 3 5 6 7 8} 
               :iterations    #{1 2 3 4} 
               :steps         #{-4 -3 -1 1 3 4}
               :cycle-steps   #{-3 -2 -1 1 2 3}}))

(def notes 
  (prob-rythmn-step-pattern 
    {:picker picker
     ; :rvals [1/4 1/2] 
     :prob-rvals {1/4 1} 
     :start-pos (g-pos 0 0 0) 
     :end-pos (g-pos 8 0 0 )
     :bounds [:C0 :C2] 
     :start-pitch :C1 }))

; (def notes 
;   (rythmic-prob-step
;     {:prob-map {:4th-u 0.4
;                 :4th-d 0.4
;                 :5th-u 0.4
;                 :5th-d 0.4}
;      :rvals [1/4] 
;      :start-pos (g-pos 0 0 0) 
;      :end-pos (g-pos 8 0 0 )
;      :bounds [:C0 :A2] 
;      :start-pitch :C1 }))

; (pp (map #(map (c :name :pitch) %) (partition 8 8 notes)))
(def notes (map #(assoc % :velocity (rand-int-between 50 80)) notes))

(def chords 
  (loop-line 8
    (ap m-note-line-from 
     (g-pos 0 0 0) 4 40 1
     (map #(a p-chord %) 
      [[:C-1 :P5 :M6 :M2-u1 :+4-u1]
       [:Ab-2 :P5 :M6 :M2-u1 :+4-u1]
       ; [:C-1 :M6 :M7 :M2-u1 :m3-u1 :P5-u1]
       ; [:B-2 :M7 :M3-u1 :+5-u1]
       ; [:B-2 :P4 :m7 :M2-u1 :M3-u1]
       ; [:A-2 :m6 :m3-u1 :P5-u1 :m7-u1 :M2-u2]
       ; [:F-1 :P5 :M2-u1 :m3-u1 :M7-u1]
       ]))))
 
; (def basses (loop-line 2 (ap m-note-line-from (g-pos 0 0 0) 4 60 1
;                              [:C-2 :C-2 :C-2 :B-3 :B-3 :A-3 :F-2])))

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn go [] (play vep (concat chords notes)))

;******************* Markov-analysis tests ************************

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
  (->> (:1 rmmlo)
       (markov-depth-analysis  3 [1 2 3])
       (markov-chain len 90)
       (map pitch)
       (ap m-note-line-from (g-pos 0 0 0) 1/4 60 1)))

(defn mc2 [len]
  (->> (:2 rmmlo)
       (markov-depth-analysis  3 [1 2 3])
       (markov-chain len 61)
       (map pitch)
       (ap m-note-line-from (g-pos 0 0 0) 1/4 40 1)))

(defn mc [] (play vep (concat (mc1 100) (map #(update-in % [:pitch] transpose (c-interval :P5-d1)) (mc1 100)))))
