(ns bartok.midi.xml-parser
  (:use utils.all)
  (:use bartok.litterals.all)
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml])
  (:use [clojure.data.zip.xml]))

(def score (zip/xml-zip (xml/parse (java.io.File. "src/midi-files/noct21.xml"))))

(def- int->key 
  {0 :C -1 :Bb -2 :Bb -3 :Eb -4 :Ab 
   -5 :Db -6 :Gb -7 :Cb 1 :G 2 :D 
   3 :A 4 :E 5 :B 6 :F# 7 :C#})

(defn- attributes [loc] 
  
  (let [key (first (xml-> loc :key :fifths text))
        mod (first (xml-> loc :key :mode text))
        beats (first (xml-> loc :time :beats text))
        beat-type (first (xml-> loc :time :beat-type text))
        divs (first (xml-> loc :divisions text))]
    {:key  [(when key (int->key (parse-int key)))
            (when mod (keyword mod))]
   :time (when (and beats beat-type) 
           (time-signature (parse-int beats)(parse-int beat-type)))
   :divisions (when divs (parse-int divs))}))

(defn- note [loc]
  (let [step   (first (xml-> loc :pitch :step text))
        octave (first (xml-> loc :pitch :octave text))
        alter  (first (xml-> loc :pitch :alter text))
        dur    (first (xml-> loc :duration text))
        typ    (first (xml-> loc :type text))
        staff  (first (xml-> loc :staff text))
        voice  (first (xml-> loc :voice text))]
    
    {:pitch (if step 
              (kwcat 
                (symbol step)
                (:name (alteration (if alter (parse-int alter) 0))) 
                (parse-int octave))
              :none)
     :staff (when staff (parse-int staff)) 
     :voice (when voice (parse-int voice))  
     :duration (when dur (parse-int dur))
     :type typ}))

(defn- tempo [loc]
  {:bpm (parse-int (first (xml-> loc :per-minute text)))
   :unit (keyword (first (xml-> loc :beat-unit text)))})

(defn- measure [loc]
  {:attributes (map attributes (xml-> loc :attributes))
   :tempo (map tempo (xml-> loc :direction :direction-type :metronome))
   :notes (map note (xml-> loc :note))})

(defn main []
  (let [data {:measures (map measure (xml-> score :part :measure))}
        divs (-> data :measures first :attributes first :divisions)]
    (assoc data :measures 
      (map (fn [measure] 
              (assoc measure :notes (map #(assoc % :duration (/ (:duration %) divs)) 
                                         (:notes measure))))
           (:measures data)))))
