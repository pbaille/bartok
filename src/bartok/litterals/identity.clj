(ns bartok.litterals.identity
  (:use [utils.utils])
  (:use [bartok.litterals.patterns]))

(defn- fit? [regex str]
  (if (re-matches regex str) true false))

(defn b? [x]
  (when (named? x)
    (let [n (name x)
          c (count n)]
      (cond
        (fit? alteration-pat n) :alteration
        (fit? direction-pat n) :direction
        (and (symbol? x) (fit? natural-pitch-class-pat n)) :natural-pitch-class
        (fit? degree-pat n) :degree
        (fit? pitch-class-pat n) :pitch-class
        (fit? pitch-pat n) :pitch
        (fit? interval-pat n) :interval
        (fit? mode-class-pat n) :mode-class
        (fit? mode-pat n) :mode
        (fit? generic-interval-class-pat n) :generic-interval-class
        (fit? generic-interval-pat n) :generic-interval
        (fit? degree-class-pat n) :degree-class
        (fit? h-function-pat n) :h-function
        (fit? time-signature-pat n) :time-signature
        :else nil))))

(defn b-type [x & more]
  (cond
    (named? x) (or (b? x) (type x))
    (number? x) (if (ratio? x) :ratio :number)
    :else (type x)))

(defn b-types 
  ([arg] (b-type arg))
  ([arg & more] (vec (map b-type (concat [arg] more)))))
