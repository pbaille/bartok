(ns bartok.litterals.identity
  (:use [utils.utils]))


(defn natural-pitch-class-name? [x] 
  (if (named? x) (re-matches #"[A-G]" (name x))))

(defn alteration-name? [x] 
  (if (named? x) (re-matches #"[x#b]b*" (name x))))

(defn pitch-name? [x] 
  (if (named? x)
    (if-let [[_ nat alt oct] (re-matches #"([A-G])([x#b]b*)*(\-*[0-5])" (name x))]
      {:pitch-class (keyword (str nat alt)) :octave (parse-int oct)})))

(defn pitch-class-name? [x]
  (if (named? x)
    (if-let [[_ nat alt] (re-matches #"([A-G])([x#b]b*)*" (name x))]
      {:base nat :alteration alt})))

(defn generic-interval-class-name? [x] 
  (if (named? x) 
    (re-matches #"unison|second|third|fourth|fifth|sixt|seventh" (name x))))

(defn interval-class-name? [x] 
  (if (named? x) (re-matches #"[mM#][23]|[bP+][45]|[mM]6|[omM]7" (name x))))

(defn interval-name? [x]
  (if (named? x) (re-matches #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)([ud][0-5]*)" (name x))))

(defn mode-class-name? [x] 
  (if (named? x) 
    (re-matches #"Lyd#2|AltDim|Harmm|Loc6|Ion\+|Dor\+4|PhryM|Lyd\+|Lydb7|Mixb6|Loc2|Alt|Melm|Phry6|Lyd|Mix|Eol|Loc|Ion|Dor|Phry" (name x))))

(defn mother-mode-name? [x]
  (if (named? x) 
    (re-matches #"Lyd#2|Lyd\+|Lyd" (name x))))

(defn split-mode-name [x]
  (if (named? x) 
    (let [[r m] (map keyword (clojure.string/split (name x) #"\-"))]
      (if (and (pitch-class-name? r) (mode-class-name? m)) [r m] nil))))

(defn mode-name? [x]
  (if (split-mode-name x) true false))

; (defn mode-name? [x] 
;   (let [[r m] (map keyword (clojure.string/split (name x) #"\-"))]
;     (if (and (abs-mode-name? m) (pitch-class-name? r)) [r m] nil)))

