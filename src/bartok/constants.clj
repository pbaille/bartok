(ns bartok.constants
  (:use [utils.utils]))

;***************



; (def m-degree-dist 
;   (reduce #(into %1 (clojure.set/map-invert (second %2))) {} m-degrees))

; (def m-degree-generic 
;   (reduce 
;     #(into %1 (apply hash-map 
;                 (mapcat 
;                   (fn [a b] (let [a (keyword a)] [a b])) 
;                   (vals (second %2)) 
;                   (repeat (first %2)))))
;     {} m-degrees))

; (def m-degree-generic-defaults 
;   {:root :R :second :M2 :third :M3 :fourth :P4 :fifth :P5 :sixth :M6 :seventh :M7})

; (def m-degree-dist-defaults 
;   [:R :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7])

; (defn m-degree-name? [n] (in? (keys m-degree-dist) (keyword n)))
; (defn m-degree-generic-name? [n] (in? (keys m-degrees) (keyword n)))

;***************

(def mother-modes
  {:Lyd {
      :degrees [:M2 :M3 :+4 :P5 :M6 :M7]
      :childs [:Lyd :Mix :Eol :Loc :Ion :Dor :Phry]
      :modes_prio 
        [[:+4 :M7 :M3 :M6 :M2 :P5]
         [:m7 :P4 :M3 :M6 :M2 :P5]
         [:m6 :M2 :P5 :m3 :m7 :P4]
         [:b5 :m2 :m7 :m6 :m3 :P4]
         [:M7 :P4 :M3 :M6 :M2 :P5]
         [:M6 :m3 :m7 :M2 :P5 :P4]
         [:m2 :P5 :P4 :m7 :m3 :m6]]}
   :Lyd+ {
      :degrees [:M2 :M3 :+4 :+5 :M6 :M7]
      :childs [:Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6]
      :modes_prio 
        [[:+5 :M7 :M3 :+4 :M6 :M2]
         [:+4 :m7 :M3 :M6 :M2 :P5]
         [:m6 :M3 :M2 :P5 :P4 :m7]
         [:b5 :M2 :m3 :m7 :P4 :m6]
         [:b4 :m7 :m6 :m3 :b5 :m2]
         [:M7 :m3 :M6 :M2 :P5 :P4]
         [:M6 :m2 :P4 :m7 :m3 :P5]]}
   :Lyd+9 {
      :degrees [:#2 :M3 :+4 :P5 :M6 :M7]
      :childs [:Lyd+9 :AltDim :Harmm :Loc6 :Ion+ :Dor#4 :PhryM]
      :modes_prio 
        [[:+4 :#2 :M7 :M3 :M6 :P5]
         [:b4 :o7 :m2 :b5 :m6 :m3]
         [:m6 :M7 :M2 :m3 :P5 :P4]
         [:b5 :M6 :m2 :m7 :m3 :P4]
         [:P4 :+5 :M7 :M3 :M2 :M6]
         [:M6 :+4 :m3 :M2 :m7 :P5]
         [:m2 :M3 :P5 :m7 :m6 :P4]]}})
   
;hash-map {mode-name mother-name}
(def mother 
  (apply hash-map 
    (mapcat 
     (fn [[mother-name fields]] 
       (mapcat 
        #(list % mother-name) 
        (:childs fields)))
     mother-modes)))

(def modal-moves 
  {:SD 5 :SD- 8 :SD+ 2 :SDalt 11 
   :T  0 :T-  3 :T+  9 :Talt  6})


