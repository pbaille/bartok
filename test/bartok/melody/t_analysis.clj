(ns bartok.melody.t-analysis
  (:use midje.sweet)
  (:use bartok.composition.utils)
  (:use bartok.primitives)
  (:use bartok.structure.position)
  (:use utils.all)
  (:use bartok.melody.analysis))

(grid)
(def- notes (note-line-from (g-pos 0 0 0) 1 [:C0 :D0 :Ab0]))
(def- notes2 (note-line-from (g-pos 0 0 0) 1/4 
                [:C0 :D0 :Eb0 :G0 :Ab0 :Bb0 
                 :C0 :D0 :C#0 :D0 :C0 :D0 
                 :Eb0 :Ab0 :Bb0 :C0 :D0 
                 :C#0 :D0 :C0 :D0 :Eb0 :Ab0 
                 :Bb0 :C0 :D0 :C#0 :D0]))

(fact "extract pitch classes"
  (extract-pitch-classes notes) => #{:C :D :Ab})

(fact "find-mothers"
  (find-mothers notes) => [:Ab-Lyd :Ab-Lyd+ :Gb-Lyd+ :Ab-Lyd#2])