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
      (as>> (bartok.rythmn.pattern/rand-rythmic-cell [1 1/4 2/3 1/3] 6 3)
        (repeat 4)
        flatten
        timable-queue
        (map #(note %2 (:duration %) (:position %)) _ (cycle [:C0 :A0])))
      pattb
      (as>> (bartok.rythmn.pattern/rand-rythmic-cell [1 1/4 2/3 1/3] 8 4)
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

(comment 
  (let [rc-picker 
        (rythmic-cell-picker
          {:rvals [1 1/3 2/3]
           :lengths #{5 6 7}
           :density 0.5})
        rc-picker2 
        (rythmic-cell-picker
          {:rvals [1 1/3 2/3]
           :lengths #{5 6 7}
           :density 0.5})
        patta
        (as>> (rc-picker)
          (repeat 4)
          flatten
          timable-queue
          (map #(note %2 (:duration %) (:position %)) _ (cycle [:C0 :G0])))
        pattb
        (as>> (rc-picker2)
          (repeat 4)
          flatten
          timable-queue 
          (map #(note %2 (:duration %) (:position %)) _ (cycle [:Eb1 :A1])))]
    (play *m-out* (concat patta pattb))))

(def rc (rythmic-cell-picker
          {:rvals [1 1/3 2/3]
           :lengths #{6}
           :density 0.5}))

;;;;;;;;;;;;; rythmn gen ;;;;;;;;;;;;;;

(use 'bartok.rythmn.skull)
(comment
  (let [sk (r-skull 
              16
              {:complexity 1/4
               :r-bases-prob-map {2 1 3 0.8} 
               :poly-homogeneity 0.2})
        patt (as>> (skull-fill sk 
                      {:mean-speed 3/4
                       :homogeneity 0.1
                       :polarity 0.5})
                     timable-queue
                     (map #(note %2 (:duration %) (:position %)) _ (repeatedly #(pitch (rand-int-between 60 72)))))
        met (->> (repeater [[16 1]])
                 timable-queue
                 (map #(note :C-1 1/16 (:position %) 30 1)))]
    (play *m-out* (concat patt met))))

