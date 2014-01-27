(ns bartok.composition.rythmic-step-pattern
  (:use utils.all)
  (:use bartok.types.note)
  (:use bartok.primitives)
  (:use bartok.melody.all)
  (:use bartok.harmony.all)
  (:use bartok.rythmn.all)
  (:use bartok.structure)
  (:use [bartok.composition.utils]))

(defn rythmic-step-pattern [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line rvals start-pos end-pos)]
    (step-patternify rl picker bounds start-pitch)))

; (def picker (lazy-step-pattern-picker 
;               {:cycle-lengths #{4 3 5 6 7 8} 
;                :iterations    #{1 2 3 4} 
;                :steps         #{-4 -3 -1 1 3 4}
;                :cycle-steps   #{-3 -2 -1 1 2 3}}))

; (def notes 
;   (rythmic-step-pattern 
;     {:picker picker 
;      :rvals [1/4] 
;      :start-pos (g-pos 0 0 0) 
;      :end-pos (g-pos 2 0 0 )
;      :bounds [:C0 :C2] 
;      :start-pitch :C1 }))

(defn prob-rythmn-step-pattern [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [picker prob-rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-prob-line prob-rvals start-pos end-pos)]
    (step-patternify rl picker bounds start-pitch)))

; (def picker (lazy-step-pattern-picker 
;               {:cycle-lengths #{4 3 5 6 7 8} 
;                :iterations    #{1 2 3 4} 
;                :steps         #{-4 -3 -1 1 3 4}
;                :cycle-steps   #{-3 -2 -1 1 2 3}}))

; (def notes 
;   (prob-rythmn-step-pattern 
;     {:picker picker 
;      :prob-rvals {1/4 1 1/6 1/4} 
;      :start-pos (g-pos 0 0 0) 
;      :end-pos (g-pos 8 0 0 )
;      :bounds [:C0 :C2] 
;      :start-pitch :C1 }))

(defn rythmic-prob-step [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [prob-map rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line rvals start-pos end-pos)
        hc (map #(-> % (dissoc :elements) (assoc :steps (count (:elements %)))) (harmonic-chunks rl))
        gi-seq (reductions #(interval-prob-line 
                              (melodic-domain (:mode %2) bounds (-> % :domain :current :pitch)) 
                              prob-map (:steps %2)) 
                           (melodic-domain (-> hc first :mode) bounds start-pitch)
                           hc)]
    (map #(note %1 (:duration %2) (:position %2)) (mapcat :pitches gi-seq) rl)))

; (def notes 
;   (rythmic-prob-step
;     {:prob-map {:4th-u 0.4
;                 :4th-d 0.4
;                 :5th-u 0.4
;                 :5th-d 0.4
;                 :2nd-d 1
;                 :2nd-u 1}
;      :rvals [1/2] 
;      :start-pos (g-pos 0 0 0) 
;      :end-pos (g-pos 2 0 0 )
;      :bounds [:C0 :C2] 
;      :start-pitch :C1 }))

; (defn rythmn-first [options-map]
;   (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
;         {:keys [picker prob-rvals bounds start-pitch start-pos end-pos]} options-map]
;     ))


; (rythmn-first
;   {:r [r-prob-line {1/2 1 1/4 1}]
;    :m [step-pattern ]
;    :start [0 0 0]
;    :end   [1 0 0]
;    :m-bounds [:C-1 :C1]
;    :start-pitch :C0})

