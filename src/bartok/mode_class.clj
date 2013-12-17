(ns bartok.mode-class
  (:use [bartok.litterals.identity])
  (:use [bartok.interval-class])
  (:use [bartok.pitch-class])
  (:use [bartok.pitch])
  (:use [utils.utils]))

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
   :Lyd#2 {
      :degrees [:#2 :M3 :+4 :P5 :M6 :M7]
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
            degree (->> moth-hash :childs (pos #{ch-name}) first inc)
            prio   (map interval-class 
                        (-> moth-hash :modes_prio (nth (dec degree))))
            degrees (vec (sort-by :val prio))]
        {:name ch-name :mother moth-name :degree degree :prio prio :degrees degrees}))
    mode-class->mother)))

(def name->mode-class (reduce #(into %1 {(:name %2) %2}) {} mode-classes))

(def mother-degree->mode-class 
  (reduce #(into %1 {{:mother (:mother %2) :degree (:degree %2)} %2}) {} mode-classes))

;;***********************************
 
(defrecord ModeClass [name mother degree prio degrees])


;; type check
(defn mode-class? [x] (instance? ModeClass x))

;;*********** Constructor ***********

(defn map->ModeClass [m] 
  (->ModeClass (:name m) (:mother m) (:degree m) (:prio m) (:degrees m)))

(defmulti mode-class
  (fn [& args]
    (let [[a b] args]
      (cond
        (and (mode-class-name? a) (not b)) 
          :name
        (and (mother-mode-name? a) (number? b)) 
          [:mother :degree]))))

(defmethod mode-class :name [n] (map->ModeClass (name->mode-class n)))
(defmethod mode-class [:mother :degree] [m d] (map->ModeClass (mother-degree->mode-class {:mother m :degree d})))







