(in-ns 'bartok.core)

(def dpc (d-passing-context (w-mode :D-Melm [:M2 :M6 :M7])))

(grid {:bars [[32 :4|4]] :tempo 120})

;single passing 
(comment 
  (let [tar (rand-nth [:E1 :G#1 :B1])]
    (->> (make-passing dpc tar 3 true)
         (cons tar)
         (m-note-line-from (g-pos) 1/2 60 1)
         (play @*midi-out*))))

(comment 
  (play-pitch-line
    (lazy-drunk-passing-line
      (w-mode :C-Phry6)
      [:D-1 :D2]
      {:2nd-u 1 :2nd-d 1 
       :3rd-u 0.5 :3rd-d 0.5 
       :4th-u 0.2 :4th-d 0.2
       :5th-u 0.2 :5th-d 0.2}
      {0 0, 1 1, 2 0, 3 1/2}
      1/2
      100)))




