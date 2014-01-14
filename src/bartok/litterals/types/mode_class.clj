(in-ns 'bartok.litterals.types)

(load "types/degree")
(load "types/pitch_class")
(load "types/pitch")


(def mother-modes
  {:Lyd {
      :degrees [:P1 :M2 :M3 :+4 :P5 :M6 :M7]
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
      :degrees [:P1 :M2 :M3 :+4 :+5 :M6 :M7]
      :childs [:Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6]
      :modes_prio 
        [[:+5 :M7 :M3 :+4 :M6 :M2]
         [:+4 :m7 :M3 :M6 :M2 :P5]
         [:m6 :M3 :M2 :P5 :P4 :m7]
         [:b5 :M2 :m3 :m7 :P4 :m6]
         [:b4 :m7 :m6 :m3 :b5 :m2]
         [:M7 :m3 :M6 :M2 :P5 :P4]
         [:M6 :m2 :P4 :m7 :m3 :P5]]}
   :Lyd#2 {
      :degrees [:P1 :#2 :M3 :+4 :P5 :M6 :M7]
      :childs [:Lyd#2 :AltDim :Harmm :Loc6 :Ion+ :Dor+4 :PhryM]
      :modes_prio 
        [[:+4 :#2 :M7 :M3 :M6 :P5]
         [:b4 :o7 :m2 :b5 :m6 :m3]
         [:m6 :M7 :M2 :m3 :P5 :P4]
         [:b5 :M6 :m2 :m7 :m3 :P4]
         [:P4 :+5 :M7 :M3 :M2 :M6]
         [:M6 :+4 :m3 :M2 :m7 :P5]
         [:m2 :M3 :P5 :m7 :m6 :P4]]}})
   
;hash-map {mode-name mother-name}
(def mode-class->mother 
  (apply hash-map 
    (mapcat 
     (fn [[mother-name fields]] 
       (mapcat 
        #(list % mother-name) 
        (:childs fields)))
     mother-modes)))

; (defn mode? [m] (= (set (keys m)) #{:name :mother :degree :degrees :prio}))

(def mode-classes
  (set (map
    (fn [[ch-name moth-name]]
      (let [moth-hash (mother-modes moth-name)
            deg-index (->> moth-hash :childs (index-of ch-name))
            deg (degree (nth (:degrees moth-hash) deg-index))
            prio   (map degree (-> moth-hash :modes_prio (nth deg-index)))
            degrees (cons (degree :P1) (vec (sort-by :val prio)))]
        (with-type 
          'ModeClass
          {:name ch-name :mother moth-name :degree deg :prio prio :degrees degrees})))
    mode-class->mother)))

(def name->mode-class (reduce #(into %1 {(:name %2) %2}) {} mode-classes))

;;*********** Constructor ***********

(defmulti mode-class b-types )

(defmethod mode-class :mode-class [n] (name->mode-class n))
(defmethod mode-class [:mode-class :number] [m d] 
  (mode-class (-> mother-modes m :childs (nth (dec d)))))







