(in-ns 'bartok.core)

(grid {:bars [[2 :4|4]] 
       :tempo 120 
       :harmony {[0 0] :C-Lyd
                 [1 0] :Ab-Lyd}})

(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{3} 
               :iterations    #{3} 
               :steps         #{-4 -3 -1 1 3 4}
               :cycle-steps   #{-7 -3 -2 -1 1 2 3 7}}))

(def pos-durs
  (->> (bartok.rythmn.pattern/rythmic-cells [1/2 1/4] 8 3)
       rand-nth
       (repeat 10)
       flatten
       (map #(hash-map :duration %))
       timable-queue))

(def clyd (melodic-domain :C-Lyd [:C0 :C1]))

(comment 
  (play *m-out*
   (step-patternify 
    pos-durs
    picker
    [:C0 :C2]
    :C1)))

(let [patta
      (as>> (bartok.rythmn.pattern/rand-rythmic-cell [2/3 1/3] 6 3)
        (repeat 4)
        flatten
        timable-queue
        (map #(note %2 (:duration %) (:position %)) _ (cycle [:C0 :A0])))
      pattb
      (as>> (bartok.rythmn.pattern/rand-rythmic-cell [1] 4 4)
        (repeat 3)
        flatten
        timable-queue 
        (map #(note %2 (:duration %) (:position %)) _ (cycle [:Eb0 :B0 :G0])))]
  (play *m-out* (concat patta pattb)))
 
 (comment 
   (->> (bartok.rythmn.pattern/rythmic-cells [1/2 3/4 1/4] 8 3)
     rand-nth
     (repeat 4)
     flatten
     (map #(hash-map :duration %))
     timable-queue
     play-rythmic-line))


