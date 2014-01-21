(ns vendors.musicxml
  (:use utils.utils)
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml])
  (:use [clojure.data.zip.xml]))
 
(defn measure-label [loc]
  [ "part" (xml1-> loc zip/up (attr :id))
    "bar" (xml1-> loc (attr :number))])
 
(defn pitch [loc]
  (keyword
    (str
      (xml1-> loc :pitch :step text)
      (if-let [alter (xml1-> loc :pitch :alter text)]
        (condp = alter
          "-1" "b"
          "1" "#"
          (str "[unknown " alter "]")))
      (xml1-> loc :pitch :octave text))))
 
(defn note-detail [loc]
  (if (xml1-> loc :pitch)
    [ " " (pitch loc) ]
    [" nil"]))
 
(defn duration [loc]
  (let [dur (xml1-> loc :duration text)]
    (condp = dur
      "1" :SQ
      "2" :Q
      "3" :DQ
      "4" :C
      "6" :DC
      "8" :M
      "16" :SB
       dur)))
 
(defn gather-chord
  ([loc] (gather-chord loc []))
  ([loc result]
    (let [chord (conj result (note-detail loc))
          next-note (xml1-> loc zip/right [:chord])]
      (if next-note
        (recur next-note chord)
        chord))))
 
(defn chord-start [loc]
  [ " [[" (gather-chord loc) "] " (duration loc) "]\n"])
 
(defn note [loc]
  (if (xml1-> loc :chord)
    ""
    (chord-start loc)))
 
(defn measure [loc]
  [ "\n"
    "(def " (measure-label loc) "\n"
    "  [" (map note (xml-> loc :note)) "])\n"
  ])
 
(defn measure-ref [loc]
  ["    " (measure-label loc) "\n"])
 
(defn part [loc]
  [ "\n"
    "(def part" (xml1-> loc (attr :id)) "\n"
    "  (concat\n"
    (map measure-ref (xml-> loc :measure))
    "))\n"])
 
(defn parts1 [z]
  (map measure (xml-> z :part :measure)))
 
(defn parts2 [z]
  (map part (xml-> z :part)))
 
(defn parts
"Creates a nested vector of strings with the note and chord
 information from a MusicXML file that has been parsed and zipped.
 
 e.g. (parts (zip/xml-zip (xml/parse (java.io.File. \"data/dilly2.xml\"))))
 
 To convert to a string, #(apply str (flatten %))
"
  [z]
  [(parts1 z) (parts2 z)])

