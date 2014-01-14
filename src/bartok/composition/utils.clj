(ns bartok.composition.utils
  (:use bartok.structure.position)
  (:use bartok.types.note)
  (:use utils.utils)
  (:use vendors.debug-repl)
  (:use bartok.litterals.all))

(b-fn note-line-from 
  [pos dur & notes-and-chords]
  (reduce #(let [l (last %1)
                 l-pos (cond (nil? l) (pos- pos dur)
                             (vector? l) (-> l last :position) 
                             :else (:position l))
                 next-pos (pos+  l-pos dur)]
             (conj %1 (if (type= %2 'Pitch)
                        (note %2 dur next-pos)
                        (chord %2 dur next-pos)))) 
          [] notes-and-chords))

(b-fn m-note-line-from 
  [pos dur vel chan & notes-and-chords]
  (map #(assoc % :channel chan :velocity vel) 
       (ap note-line-from pos dur notes-and-chords)))

;pitch vectors represent chords
;(note-line-from (g-pos 0 0 0) 1 [:C#1 :D2] :C#0 :A2 [:C#1 :E1])

(defn loop-line [n line]
  (let [end-pos-val (pos-val (pos+ (:position (last line))(:duration (last line))))]
    (mapcat (fn [[n line]]
              (map #(update-in % [:position] pos+ (* n end-pos-val)) line)) 
            (for [nn (range n)] [nn line]))))


(defn harmonic-chunks 
  "takes a position-sorted coll of {:position _ :duration _ ...} map and chunk it by harmony"
  [r-line]
  (let [harmonized (map #(assoc % :mode (:name (mode-at (pos+ (:position %) (- (:duration %) 1/100))))) r-line)
        chunked (map #(hash-map :mode (:mode (first %)) 
                                :elements (map (fn [e] (dissoc e :mode)) %)) 
                     (partition-by :mode harmonized))]
    chunked))

; (harmonic-chunks [{:position {:cycle 0, :bar 0, :sub 0}, :duration 1/2}
;                   {:position {:cycle 0, :bar 0, :sub 1N}, :duration 1/2}
;                   {:position {:cycle 0, :bar 0, :sub 7/2}, :duration 1/2}
;                   {:position {:cycle 0, :bar 1, :sub 0N}, :duration 1/2}
;                   {:position {:cycle 0, :bar 1, :sub 3/2}, :duration 1/2}
;                   {:position {:cycle 0, :bar 1, :sub 2N}, :duration 1/2}])

; [{:mode :C-Lyd+,
;   :elements
;   [{:position [0 0 0], :duration 1/2}
;    {:position [0 0 1N], :duration 1/2}
;    {:position [0 0 7/2], :duration 1/2}]}
;  {:mode :Ab-Lyd+,
;   :elements
;   [{:position [0 1 0N], :duration 1/2}
;    {:position [0 1 3/2], :duration 1/2}
;    {:position [0 1 2N], :duration 1/2}]}]

