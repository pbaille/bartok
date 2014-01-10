(ns bartok.melody.strategies
  (:use [bartok.types])
  (:use [bartok.melody.melodic-domain])
  (:use [bartok.litterals.evaluation])
  (:use [utils.prob])
  (:use [utils.utils]))

(def ^:private default-prob-map 
  (apply hash-map 
    (mapcat #(list % 1) (b> :2nd-u :2nd-d 
                            :3rd-u :3rd-d
                            :4th-u :4th-d))))

(defn- bartokize-prob-map [m]
  (zipmap (map b> (keys m)) (vals m)))

;prob-map keys (GenericInterval) vals (prob(number))
(defn prob-line 
  ([length] (prob-line default-prob-map length))
  ([prob-map length] ((weight-picker prob-map) length)))

(defn- choose-next-interval 
  [domain prob-map]
    (let [bounds (map :val (interval-bounds domain))
          pm (filter 
               #(-> % first :val (between (vec bounds)))
               prob-map)]
      (weight-pick-one pm)))

(defn interval-prob-line [domain prob-map length]
  (let [prob-map (bartokize-prob-map prob-map)] 
    (loop [dom domain res [] l length]
      (if (not= 0 l)
        (let [interval (choose-next-interval dom prob-map)
              dom (step dom interval)
              res (conj res interval)]
          (recur dom res (- l 1)))
        {:domain dom :pitches (step-sequence domain res)}))))

