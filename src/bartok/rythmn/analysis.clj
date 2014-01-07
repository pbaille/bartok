(ns
  #^{:doc "positioned rvals line analysis"} 
  bartok.rythmn.analysis
  (:use utils.utils)
  (:use bartok.structure.position)
  (:use bartok.melody.melodic-domain))

(defn global-bounds [start-pos end-pos bounds start-pitch]
  (let [modes (modes-between start-pos end-pos)
        domains (->> modes (map #(melodic-domain % bounds start-pitch)))
        domains-bounds (map #(map :val (interval-bounds %)) domains)]
  [(best > (map first domains-bounds)) (best < (map second domains-bounds))]))