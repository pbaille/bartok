(in-ns 'bartok.core)

(grid {:bars [[2 :4|4]] 
       :tempo 120 
       :harmony {[0 0] :C-Lyd
                 [1 0] :Ab-Lyd}})

(def picker (lazy-step-pattern-picker 
              {:cycle-lengths #{4} 
               :iterations    #{2 3 4} 
               :steps         #{-4 -3 -1 1 3 4}
               :cycle-steps   #{-7 -3 -2 -1 1 2 3 7}}))

(def pos-durs
  (->> (bartok.rythmn.pattern/rythmic-cells [1/2 1/3 1/4] 12 4)
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


(defn metro [len res pitch]
  (map #(note pitch 1/32 (:position %) 30 1)
       (timable-queue (repeat (/ len res) res))))

(defn r-gen 
  [len 
   {cp :complexity
    pp :poly-prob 
    ph :poly-homogeneity
    md :mean-duration
    h  :homogeneity
    p  :polarity 
    :as options}]
  (let [skull (r-skull len options)
        durs  (skull-fill skull options)]
    (timable-queue durs)))

(defn skull-patt
  "make pattern based on the skull strategy, 
  options are the same as above r-gen" 
  [cycle-len iterations options]
  (let [skull (r-skull cycle-len options)
        durs  (skull-fill skull options)
        exp-iters (a concat (repeat iterations durs))]
    (timable-queue exp-iters)))

(comment
  (let [durs (r-gen 
              48
              {:complexity 1/4
               :poly-prob {2 1 3 1} 
               :poly-homogeneity 0.3
               :mean-duration 1
               :homogeneity 0.1
               :polarity 0.5})
        pitched (map
                  #(note %2 (:duration %) (:position %)) 
                   durs 
                  (repeatedly #(pitch (rand-nth [60 61 63 65 67 69 70 72]))))]
    (play *m-out* (concat pitched (metro 48 1 :C-1)))))


(comment
  (let [durs (skull-patt 6 8
              {:complexity 1/4
               :poly-prob {2 1} 
               :poly-homogeneity 0.3
               :mean-duration 1/2
               :homogeneity 0.1
               :polarity 0.5})
        pitched (map
                  #(note %2 (:duration %) (:position %)) 
                   durs 
                  (repeatedly #(pitch (rand-nth [60 61 63 65 67 69 70 72]))))]
    (play *m-out* (concat pitched (metro 48 1 :C-1)))))

(comment
  (let [durs (skull-patt 6 8
              {:complexity 1/4
               :poly-prob {2 1} 
               :poly-homogeneity 0.3
               :mean-duration 1/2
               :homogeneity 0.1
               :polarity 0.5})
        pitched (map
                  #(note %2 (:duration %) (:position %)) 
                   durs 
                  (repeatedly #(pitch (rand-nth [60 61 63 65 67 69 70 72]))))
        durs2 (skull-patt 4 12
              {:complexity 1/3
               :poly-prob {3 1} 
               :mean-duration 1.5
               :homogeneity 0.5
               :polarity 0.8})
        pitched2 (map #(note :C-1 1/32 (:position %) 30 1) durs2 )]
    (play *m-out* (concat pitched pitched2))))
