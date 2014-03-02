(ns bartok.harmony.chord-notation
  (:use utils.all)
  (:use [bartok primitives print])
  (:require [clojure.string :as s]))

(def known-chords {
  :M      [:M3 :P5]
  :m      [:m3 :P5]
  :+      [:M3 :+5]
  :o      [:m3 :b5]
  :7      [:M3 :P5 :m7]
  :M7     [:M3 :P5 :M7]
  :m7     [:m3 :P5 :m7]
  :mM7    [:m3 :P5 :M7]
  :m7b5   [:m3 :b5 :m7]
  :mM7b5  [:m3 :b5 :M7]
  :o7     [:m3 :b5 :o7]
  :M7+    [:M3 :+5 :M7]
  :7+     [:M3 :+5 :m7]}) 

(def known-chords-syns
 (reduce-kv
   #(a assoc %1 (mapcat (fn [x] (vector x (known-chords %2))) %3))
   {}  
   {:M      [:M :maj :Maj :major :Major]
    :m      [:- :m :min :Min :minor :Minor]
    :+      [:+ :aug :Aug]
    :o      [:o :dim :Dim]
    :7      [:7 :seventh :dominant :Dominant]
    :M7     [:M7 :maj7 :Maj7 :major7 :Major7 :∆ :∆7]
    :m7     [:m7 :-7 :min7 :Min7]
    :mM7    (for [x [:m :min :Min :minor :Minor] 
                  y [:M7 :maj7 :Maj7 :major7 :Major7 :∆ :∆7]] 
              (kwcat x y))
    :m7b5   [:m7b5 :Ø :ø :-7b5]
    :mM7b5  [:mM7b5 :m∆b5 :m∆7b5 :-∆b5 :-∆7b5]
    :o7     [:o7    :dim7 :Dim7]
    :M7+    (for [x [:Maj7 :maj7 :∆ :∆7 :M7] y [:#5 :+5 :+]] (kwcat x y))
    :7+     (for [x [:seventh :dominant :Dominant :7] y [:#5 :+5 :+]] (kwcat x y))}))

(def cic-syns 
  (reduce-kv
   #(a assoc %1 (mapcat (fn [x] (vector x %2)) %3))
   {}  
   {:m2 [:m2 :b2 :-2 :-9 :b9 :m9]
    :M2 [:M2 :2 :P2 :9 :P9 :M9]
    :#2 [:#2 :+2 :+9 :#9]
    :o3 [:o3 :bb3 :dim3 :o10 :bb10 :dim10]
    :m3 [:m3 :b3 :-3 :m10 :b10 :-10]
    :M3 [:M3 :P3 :3 :M10 :P10 :10]
    :b4 [:b4 :-4 :m4 :-11 :b11 :m11]
    :P4 [:P4 :4 :11 :M4 :P11 :M11]
    :+4 [:+4 :#4 :#11 :+11]
    :b5 [:b5 :-5 :m5]
    :P5 [:P5 :5 :M5]
    :+5 [:+5 :#5]
    :m6 [:m6 :m13 :b13 :b6 :-13 :-6]
    :M6 [:M6 :6 :13 :P13 :P6 :M13]
    :o7 [:o7 :dim7 :bb7]
    :m7 [:m7 :-7 :b7 :7]
    :M7 [:M7 :P7 :∆] }))

(def ^:private alteration-pattern  #"(|bb|o|b|m|M|N|P|#|\+|x)")

(defn parse-chord 
  "given a chord notation return a seq of c-interval-class
  ex: (parse-chord :69) => (:M2 :P5 :M3 :M6)
      (parse-chord :m∆9) => (:M2 :P5 :M7 :m3)
      (parse-chord :sus2) => (:M2 :P5)"
  [kw]
  (let [[_ base seventh sharp-fifth sus adds]
        (re-find #"(Major|major|Minor|minor|maj|Maj|min|Min|m|M|-)?(7|ø|Ø|∆)?(\+)?(sus)?(\S*)" (name kw))
        base (if (seq (concat base seventh sharp-fifth)) 
               (known-chords-syns (kwcat base seventh sharp-fifth)) 
               [:M3 :P5]) ;defaults to Major
        base-sus (if sus (next base) base) ;if sus remove 3rd
        adds (->> (re-seq (pat-comp alteration-pattern #"(11|13|9|10|[2-7]|∆)") 
                          (s/replace adds #"\." "")) ;remove dot separators
                  (map (f> first keyword cic-syns)))
        all (concat base-sus adds)
        ; this little trick is for overiding double d-interval-class ex: (P5 b5) => (b5)
        all (a hash-map (mapcat vector (map (f> b> :d-class :val) all) all))]
    (vals all)))