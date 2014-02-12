(in-ns 'bartok.core)


(def dpc (d-passing-context (w-mode :C-Lyd+ [:M3 :+5 :M7])))

(grid)

(grid-assoc :tempo 120)

(let [tar (rand-nth [:E1 :F#1 :A1 :B1])]
  (->> (make-passing dpc tar 3 true)
       (cons tar)
       (m-note-line-from (g-pos) 1/2 60 1)
       (play @*midi-out*)))


