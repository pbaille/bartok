(ns bartok.litterals.identity
  (:use [bartok.constants])
  (:use [utils.utils]))

(defn pitch-name? [x] 
  (if-let [[_ nat alt oct] (re-matches #"([A-G])([x#b]b*)*(\-*[0-5])" (name x))]
    {:pitch-class (keyword (str nat alt)) :octave (parse-int oct)}))

(defn pitch-class-name? [x]
  (if-let [[_ nat alt] (re-matches #"([A-G])([x#b]b*)*" (name x))]
    {:base nat :alteration alt}))

(defn m-degree-name? [x] 
  (re-matches #"[mM#][23]|[bP+][45]|[mM]6|[omM]7" (name x)))


(defn interval-name? [x]
  (re-matches #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)([ud][0-5]*)" (name x)))

(defn abs-mode-name? [x] (mother x))

(defn mode-name? [x] 
  (let [[r m] (map keyword (clojure.string/split (name x) #"\-"))]
    (if (and (abs-mode-name? m) (pitch-class-name? r)) [r m] nil)))

