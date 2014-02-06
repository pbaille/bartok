(in-ns 'bartok.core)

(def mel-steps [3 3 3 -1 1 1 1 2 1 -3 -3 -2 -1 -1 2 -1 -3 -3 -2])

(def cntr-an [[4 10][4 -15][4 13][4 -8][4 10][4 -15][4 13][4 -8][4 10][4 -15][4 13][4 -8]])

(def c-lyd (melodic-domain :C-Lyd [:C-2 :C3] :C-1))

(grid {:bars [[24 :4|4]] :tempo 120})

(time (contour-prob-line 
             mel-steps 
            (zipmap (range -4 5) (repeat 1)) 
             c-lyd 
             3 ))


(time (->> (contour-prob-line2
            cntr-an 
            {-5 0.5 -4 1 -3 1 -1 0.5 
             1 0.5 3 1 4 1 5 0.5}
             c-lyd 
             2)
           (step-sequence c-lyd)
           (note-line-from (g-pos 0 0 0) 1/4)
           (play @*midi-out*)))

(time (->> (contour-prob-line 
             mel-steps 
            (zipmap (range -4 5) (repeat 1)) 
             c-lyd 
             3)
           (step-sequence c-lyd)
           (note-line-from (g-pos 0 0 0) 1/4)
           (play @*midi-out*)))

; (defn target-notes-mel [])
