(ns bartok.xml.parser
  (:use utils.all)
  (:use bartok.primitives)
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.data.xml :as dxml])
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
        divs      (xml1-> loc :divisions text)
        transd    (xml1-> loc :transpose :diatonic text)
        transc    (xml1-> loc :transpose :chromatic text)]
    {:key  (when (and key mod)
             [(int->key (parse-int key))
              (keyword mod)])
     :time  (when (and beats beat-type) 
              (time-signature (parse-int beats)(parse-int beat-type)))
     :divisions (when divs (parse-int divs))
     :transpose (when transd 
                  (c-interval (d-interval(parse-int transd)) 
                              (parse-int transc)))}))

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

;useless for now (commented in measure)
(defn- tempo [loc]
  (when (seq loc)
    {:bpm (parse-int (xml1-> loc :per-minute text))
     :unit (keyword (xml1-> loc :beat-unit text))}))

(defn sound-dir [loc]
  (some->> loc zip/node :attrs (map-vals parse-int)))

(defn- measure [loc]
  {:attributes (some-> (xml1-> loc :attributes) attributes)
   ; :tempo (or (seq (map tempo (xml-> loc :direction :direction-type :metronome)))
   ;            nil)
   :directions (a merge (map sound-dir (xml-> loc :direction :sound)))
   :notes (map note (xml-> loc :note))})

(defn- part [loc]
  (map measure (xml-> loc :measure)))

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

(defn- clean-part [part]
  (->> part 
    (map (asf> ;calc duration (unit beat)          
               (update-in _ [:notes]
                 (p map #(assoc % :duration 
                           (when (:duration %) 
                             (/ (:duration %) 
                                ;if measure contains divisions attr then use it else defer to last known
                                (or (some-> _ :attributes :divisions)
                                    (last (remnil-map (f> :attributes :divisions) part))))))))
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
                 (p map-vals add-positions))))
    (map-indexed #(vector %1 %2))
    (sort-by first)
    (a concat)
    (a sorted-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PUBLIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-mxl [mxl-path]
  (let [score (zip/xml-zip (dxml/parse-str (extract-mxl mxl-path)))
        data (map part (xml-> score :part))]
    (map clean-part data)))

(defn parse-xml [xml-path]
  (let [score (zip/xml-zip (xml/parse (java.io.File. xml-path)))
        data (map part (xml-> score :part))]
    (map clean-part data)))

