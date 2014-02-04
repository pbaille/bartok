(in-ns 'bartok.core)

(def mel-steps [3 3 3 -3 -3 -2 3 3 3 -3 -3 -2 3 3 3 -3 -3 -2])

(def c-lyd (melodic-domain :C-Lyd [:C-2 :C3] :C0))

(grid {:bars [[24 :4|4]] :tempo 120})

(time (->> (contour-prob-line 
             mel-steps 
            (zipmap (range -4 5) (repeat 1)) 
             c-lyd 
             3 )
           (step-sequence c-lyd)
           (note-line-from (g-pos 0 0 0) 1/4)
           (play @*midi-out*)))

