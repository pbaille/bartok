(ns bartok.composition.rythmic-step-pattern
  (:use bartok.structure.position)
  (:use bartok.litterals.evaluation)
  (:use bartok.note)
  (:use utils.utils)
  (:use utils.macros)
  
  (:use [bartok.melody.melodic-domain])
  (:use [bartok.melody.strategies])
  (:use [bartok.melody.step-pattern])
 
  (:use [bartok.harmony.harmony])
  (:use [bartok.harmony.h-function])
 
  (:use [bartok.rythmn.rval])
  (:use [bartok.rythmn.random-line])
  (:use [bartok.rythmn.analysis])
  
  (:use [bartok.composition.utils]))

(defn rythmic-step-pattern [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [picker rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line start-pos rvals start-pos end-pos)
        global-bounds (global-bounds start-pos end-pos bounds start-pitch)
        rl-steps (map #(assoc %1 :step %2) rl (steps-line global-bounds picker))
        hcs (map #(-> % (dissoc :elements) 
                        (assoc :steps (reduce (fn [steps {s :step}](conj steps s)) 
                                              [] (:elements %)))) 
                (harmonic-chunks rl-steps))
        pitches (step-sequence hcs bounds start-pitch)]
    (map #(note %2 (:duration %1) (:position %1)) rl pitches)))

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

(defn rythmic-prob-step [options-map]
  (let [options-map (zipmap (keys options-map) (map b> (vals options-map)))
        {:keys [prob-map rvals bounds start-pitch start-pos end-pos]} options-map
        rl (r-line start-pos rvals start-pos end-pos)
        hc (map #(-> % (dissoc :elements) (assoc :steps (count (:elements %)))) (harmonic-chunks rl))
        gi-seq (map-reduce #(interval-prob-line 
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
