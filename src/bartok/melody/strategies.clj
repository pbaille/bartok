(ns bartok.melody.strategies
  (:use bartok.melody.melodic-domain)
  (:use bartok.primitives)
  (:use [utils utils prob]))

(def ^:private default-prob-map 
  (apply hash-map 
    (mapcat #(list % 1) (b> :2nd-u :2nd-d 
                            :3rd-u :3rd-d
                            :4th-u :4th-d))))

(defn- bartokize-prob-map [m]
  (zipmap (map b> (keys m)) (vals m)))

;prob-map keys (DInterval) vals (prob(number))
(defn- prob-line 
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

(defn d-int-prob-line 
  "return a lazy d-interval seq accordingly to a melodic-domain and a {d-int prob ...} map
  
   (take 20 (d-int-prob-line 
              (melodic-domain :C-Lyd [:C0 :C2]) 
              {:2nd-u 1 :2nd-d 1
               :3rd-u 2 :3rd-d 2
               :4th-u 0.5 :4th-d 2/3}))"
  [dom prob-map]
  (letfn [(fun [domain prob-map]
            (let [interv (choose-next-interval domain prob-map)
                  dom (step domain interv)] 
              (lazy-seq (cons interv (fun dom prob-map)))))]
    (fun dom (bartokize-prob-map prob-map))))
