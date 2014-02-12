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

;little helper to save keystrokes
(defnaults play-note-line 
  [notes []
   dur 1/4
   vel 60
   chan 1
   tempo 120]
  (grid {:bars [[32 :4|4]] :tempo tempo})
  (->> notes 
      (m-note-line-from (g-pos) dur vel chan)
      (play @*midi-out*)))

(comment 
  (play-note-line [:C0 :D0 :E0]))

(b-fn drunk-passing-line
  "return a melodic line with some random passings on main degrees"
  [w-mode bounds d-int-prob-map len]
  (let [pass-cont (d-passing-context w-mode)
        main-dom (melodic-domain (:main-pitch-classes w-mode) bounds)
        dr-seq (interval-prob-line main-dom d-int-prob-map len)]
    (->> (reduce 
           (fn [acc pi] 
             (conj acc 
                   ; ensure that next passing-serie do not begins with last pitch (no repetitions!)
                   (select-first #(if (seq acc) 
                                    (not= (first %) (last (last acc)))
                                    true) 
                                 ;[1 3] shouldn't be hardcoded! it is the length of the passing-serie
                                 (pitch-passings pass-cont pi (rand-nth [1 3]))))) 
           [] (:pitches dr-seq))
         (a concat)
         (play-note-line))))

;example
(comment 
  (drunk-passing-line 
    (w-mode :C-Melm)
    [:D-1 :D2]
    {:2nd-u 1 :2nd-d 1 
     :3rd-u 0.5 :3rd-d 0.5 
     :4th-u 0.2 :4th-d 0.2}
    100))