(ns bartok.melody.strategies
  (:use bartok.melody.melodic-domain)
  (:use bartok.melody.diatonic-passing)
  (:use bartok.types.w-mode)
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
  "return a lazy pitch seq accordingly to a melodic-domain and a {d-int prob ...} map
  
   (take 20 (d-int-prob-line 
              (melodic-domain :C-Lyd [:C0 :C2]) 
              {:2nd-u 1 :2nd-d 1
               :3rd-u 2 :3rd-d 2
               :4th-u 0.5 :4th-d 2/3}))"
  [dom prob-map]
  (letfn [(fun [domain prob-map]
            (let [interv (choose-next-interval domain prob-map)
                  dom (step domain interv)] 
              (lazy-seq (cons (-> dom :current :pitch) (fun dom prob-map)))))]
    (fun dom (bartokize-prob-map prob-map))))

(b-fn lazy-drunk-passing-line
  "return a lazy melodic line with some passings on main degrees
  ex: 
  (play-pitch-line
    (lazy-drunk-passing-line
      (w-mode :C-Phry6)
      [:D-1 :D2]
      {:2nd-u 1 :2nd-d 1 
       :3rd-u 0.5 :3rd-d 0.5 
       :4th-u 0.2 :4th-d 0.2}
      {0 1, 1 1, 2 1/2, 3 1/2}
      1/2
      100))
  "
  ([w-mode bounds d-int-prob-map size-prob-map brod-rat]
  (let [pass-cont (d-passing-context w-mode)
        main-dom (melodic-domain (:main-pitch-classes w-mode) bounds)
        pitches (d-int-prob-line main-dom d-int-prob-map)]
    (a concat
       (reductions 
         (fn [acc pi] 
           ; ensure that next passing-serie do not begins with last pitch (no repetitions!)
           (select-first 
             #(if (seq acc) (not= (first %)(last acc)) true) 
             (prob-exprs 
               (- 1 brod-rat) (pitch-passings pass-cont pi (weight-pick-one size-prob-map))
               brod-rat       (map #(cons (last %) %)                            ;size 0 cause repetition when broderie
                                   (pitch-passings pass-cont pi (weight-pick-one (dissoc size-prob-map 0))))))) 
         [] pitches))))
  ([w-mode bounds d-int-prob-map size-prob-map brod-rat max-len]
   "same as above with a max-len arg 
   (max-len because last pitches can be removed if the last passing is truncated)"
  (->> (lazy-drunk-passing-line 
         w-mode 
         bounds 
         d-int-prob-map 
         size-prob-map 
         brod-rat)
       (take max-len)
       ;remove last passing if truncated
       (drop-last-while #(not (main-pitch-class? w-mode (:pitch-class %)))))))
