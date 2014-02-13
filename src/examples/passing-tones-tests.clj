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

(b-fn lazy-drunk-passing-line
  "return a lazy melodic line with some random passings on main degrees"
  ([w-mode bounds d-int-prob-map brod-rat]
  (let [pass-cont (d-passing-context w-mode)
        main-dom (melodic-domain (:main-pitch-classes w-mode) bounds)
        pitches (d-int-prob-line main-dom d-int-prob-map)]
    (a concat
       (reductions 
         (fn [acc pi] 
           ; ensure that next passing-serie do not begins with last pitch (no repetitions!)
           (select-first #(if (seq acc) 
                            (not= (first %) (last acc))
                            true) 
                         (prob-exprs 
                           ;(rand-int-between 0 4) shouldn't be hardcoded! it is the length of the passing-serie
                           (- 1 brod-rat) (pitch-passings pass-cont pi (rand-int-between 0 4))
                           brod-rat       (map #(cons (last %) %) 
                                               (pitch-passings pass-cont pi (rand-int-between 1 4)))))) 
         [] pitches))))
  ([w-mode bounds d-int-prob-map brod-rat max-len]
   "same as above with a max-len arg 
   (max-len because last pitches can be removed if the last passing is truncated)"
  (->> (lazy-drunk-passing-line w-mode bounds d-int-prob-map brod-rat)
       (take max-len)
       ;remove last passing if truncated
       (drop-last-while #(not (main-pitch-class? w-mode (:pitch-class %)))))))

(comment 
  (play-pitch-line
    (lazy-drunk-passing-line
      (w-mode :C-Phry6)
      [:D-1 :D2]
      {:2nd-u 1 :2nd-d 1 
       :3rd-u 0.5 :3rd-d 0.5 
       :4th-u 0.2 :4th-d 0.2
       :5th-u 0.2 :5th-d 0.2}
      1/2
      100)))




