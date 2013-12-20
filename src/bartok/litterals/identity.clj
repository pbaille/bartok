(ns bartok.litterals.identity
  (:use [utils.utils])
  (:use [bartok.litterals.patterns]))


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

(defn generic-interval-name? [x] 
  (if (named? x) 
    (re-matches #"(unison|second|third|fourth|fifth|sixt|seventh)([0-9])" (name x))))

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

;************************************************************

(defn- fit? [regex str]
  (if (re-matches regex str) true false))

(defn b? [x]
  (when (named? x)
    (let [n (name x)
          c (count n)]
      (cond
        (<= c 2) 
          (cond
            (fit? alteration-name n) :alteration
            (fit? natural-pitch-class-name n) :natural-pitch-class
            (fit? interval-class-name n) :interval-class
            (fit? pitch-class-name n) :pitch-class
            :else nil)
        (>= c 3)
          (cond
            (fit? pitch-class-name n) :pitch-class
            (fit? pitch-name n) :pitch
            (fit? interval-name n) :interval
            (fit? mode-class-name n) :mode-class
            (fit? mode-name n) :mode
            (fit? generic-interval-class-name n) :generic-interval-class
            (fit? generic-interval-name n) :generic-interval
            :else nil)))))

(defn b-type [x & more]
  (cond
    (named? x) (or (b? x) (type x))
    (number? x) (if (ratio? x) :ratio :number)
    :else (type x)))

(defn b-types 
  ([arg] (b-type arg))
  ([arg & more] (vec (map b-type (concat [arg] more)))))
