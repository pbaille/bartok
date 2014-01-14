(ns bartok.rythmn.weighted-line
  (:use bartok.structure.position)
  (:use bartok.rythmn.utils)
  (:use [utils utils prob]))

(def ^:private default-prob-map 
  {1/2 1 
   1 1/4 
   1/3 1/4})

(defn- prob-line 
  ([length] (prob-line default-prob-map length))
  ([prob-map length] ((weight-picker prob-map) length)))

(defn- choose-next-rval
  [pos prob-map]
    (let [pm (filter 
               #(some #{(first %)} (allowed-rvals pos (keys prob-map)))
               prob-map)
          pm (a hash-map (a concat pm))]
      (weight-pick-one pm)))

; (defn rval-prob-line [start-pos prob-map length]
;   (loop [pos start-pos res [] l length]
;     (if (not= 0 l)
;       (let [rval (choose-next-rval pos prob-map)
;             pos (pos+ pos rval)
;             res (conj res rval)]
;         (recur pos res (- l 1)))
;       res)))

(defn r-prob-line 
  ([p prob-map]
    (lazy-seq 
      (let [v (choose-next-rval p prob-map)]
        (cons {:position p :duration v} 
              (r-prob-line (pos+ p v) prob-map)))))
  ([prob-map start-pos end-pos]
     (take-while #(< (pos-val (:position %)) 
                     (pos-val end-pos)) 
                 (r-prob-line start-pos prob-map))))

