(ns bartok.xml.parser
  (:use utils.all)
  (:use bartok.primitives)
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml])
  (:use [clojure.data.zip.xml]))


; mxl decompression 
(defn extract-mxl [path]
  (let [[_ filename] (re-matches #"(.*)\.mxl$" (.getName (java.io.File. path)))
        zipfile (java.util.zip.ZipFile. path)
        zipentry (.getEntry zipfile (str filename ".xml"))
        in (.getInputStream zipfile zipentry)] 
    (slurp in)))

(def- int->key 
  {0  :C  -1 :F -2 :Bb -3 :Eb -4 :Ab -5 :Db -6 :Gb -7 :Cb 
           1 :G  2 :D   3 :A   4 :E   5 :B   6 :F#  7 :C#})

(defn- attributes [loc] 
  (let [key       (xml1-> loc :key :fifths text)
        mod       (xml1-> loc :key :mode text)
        beats     (xml1-> loc :time :beats text)
        beat-type (xml1-> loc :time :beat-type text)
        divs      (xml1-> loc :divisions text)]
    {:key  [(when key (int->key (parse-int key)))
            (when mod (keyword mod))]
     :time  (when (and beats beat-type) 
              (time-signature (parse-int beats)(parse-int beat-type)))
     :divisions (when divs (parse-int divs))}))

(defn- note [loc]
  (let [step   (xml1-> loc :pitch :step text)
        octave (xml1-> loc :pitch :octave text)
        alter  (xml1-> loc :pitch :alter text)
        dur    (xml1-> loc :duration text)
        typ    (xml1-> loc :type text)
        staff  (xml1-> loc :staff text)
        voice  (xml1-> loc :voice text)
        chord  (if (xml1-> loc :chord) true false)]
    
    {:pitch (if step 
              (kwcat 
                (symbol step)
                (:name (alteration (if alter (parse-int alter) 0))) 
                (- (parse-int octave) 5))
              :none)
     :staff (when staff (parse-int staff)) 
     :voice (when voice (parse-int voice))  
     :duration (if dur (parse-int dur) 0)
     :type typ
     :chord chord}))

(defn- tempo [loc]
  (when (seq loc)
    {:bpm (parse-int (xml1-> loc :per-minute text))
     :unit (keyword (xml1-> loc :beat-unit text))}))

(defn- measure [loc]
  {:attributes (some-> (xml1-> loc :attributes) attributes)
   :tempo (some-> (xml-> loc :direction :direction-type :metronome) tempo)
   :notes (map note (xml-> loc :note))})

(defn- group-chords [notes]
  (->> (reduce (fn [acc n]
                 (if (:chord n) 
                   (conj (vec (butlast acc)) (conj (vec-if-not (last acc)) n))
                   (conj acc n))) 
               [(first notes)] (next notes))
       ;remove chord field
       (map (fn [n] (if (vector? n) 
                      (mapv #(dissoc % :chord) n) 
                      (dissoc n :chord))))))

(defn- add-positions [notes]
  (reduce (fn [acc n]
            (let [current-pos 
                  (let [lacc (if (vector? (last acc)) 
                               (first (last acc)) 
                               (last acc))] 
                    (+ (:duration lacc) (:position lacc)))]
              (if (vector? n)
                (conj acc (mapv #(assoc % :position current-pos) n))
                (conj acc (assoc n :position current-pos))))) 
          [(if (vector? (first notes))
             (mapv #(assoc % :position 0) (first notes))
             (assoc (first notes) :position 0))] 
          (next notes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PUBLIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn main [xml-path]
  (let [score (zip/xml-zip (xml/parse (java.io.File. xml-path)))
        data {:measures (map measure (xml-> score :part :measure))}
        divs (-> data :measures first :attributes :divisions)]
    (as-> data d 
      (update-in d [:measures] 
        (p map
          (f> ;calc duration (unit beat)          
              (update-in [:notes]
                (p map #(assoc % :duration (when (:duration %) (/ (:duration %) divs)))))
              ;remove staff field (useless for now)
              (update-in [:notes]
                (p map #(dissoc % :staff)))
              ;group-by voice
              (update-in [:notes] 
                (p group-by :voice))
              ;remove voice and type fields in each note
              (update-in [:notes]
                (p map-vals (fn [v] (map #(dissoc % :voice :type) v))))
              ;rename notes field
              (clojure.set/rename-keys {:notes :voices})
              ;group chords
              (update-in [:voices]
                (p map-vals group-chords))
              ;add position field
              (update-in [:voices]
                (p map-vals add-positions))
              )))
      ;index measures as ints and sort
      (->> (map-indexed #(vector %1 %2) (:measures d))
           (sort-by first)
           (a concat)
           (a sorted-map)))))




