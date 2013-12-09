(ns bartok.litterals.identity
  (:use [bartok.constants])
  (:use [utils.utils]))

(defn pitch-kw? [x] 
  (if-let [[_ nat alt oct] (re-matches #"([A-G])([x#b]b*)*(\-*[0-5])" (name x))]
    {:pitch-class (keyword (str nat alt)) :octave (parse-int oct)}))

(defn pitch-class-kw? [x]
  (if-let [[_ nat alt] (re-matches #"([A-G])([x#b]b*)*" (name x))]
    {:base nat :alteration alt}))

(defn m-degree-kw? [x] 
  (re-matches #"[mM#][23]|[bP+][45]|[mM]6|[omM]7" (name x)))


(defn interval-kw? [x]
  (re-matches #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)([ud][0-5]*)" (name x)))

(defn abs-mode-kw? [x] (mother x))

(defn mode-kw? [x] 
  (let [[r m] (map keyword (clojure.string/split (name x) #"\-"))]
    (if (and (abs-mode-kw? m) (pitch-class-kw? r)) [r m] nil)))

