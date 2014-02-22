(in-ns 'bartok.core)

(grid {:bars [[32 :4|4]] :tempo 120})

(comment 
  "play a single passing"
  (let [dpc (d-passing-context (w-mode :C-Lyd+ [:M3 :+5 :M7]))
        tar (rand-nth [:E1 :G#1 :B1])]
    (->> (make-passing dpc tar 3 true)
         (m-note-line-from (g-pos) 1/2 60 1)
         (play *m-out*))))


(comment 
  "play a lazy-passing line"
  (play-pitch-line
    (lazy-drunk-passing-line
      ;weighted mode
      (w-mode :C-Phry6)
      ;bounds
      [:D-1 :D2]
      ;between target notes steps prob
      {:2nd-u 1 :2nd-d 1 
       :3rd-u 0.5 :3rd-d 0.5 
       :4th-u 0.2 :4th-d 0.2
       :5th-u 0.2 :5th-d 0.2}
      ;passing size prob
      {0 0, 1 0, 2 1, 3 0}
      ;broderie ratio
      1
      ;length of line
      100)))




