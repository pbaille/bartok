(in-ns 'bartok.types)

(load "types/degree")
(load "types/generic_interval")

(defn build-interval 
  ([m]
     (let [{:keys [name val direction octave-offset class generic]} m]
       (build-interval name val direction octave-offset class generic)))
  ([name val direction octave-offset class gen]
     (with-type 
       'Interval 
       {:name name 
        :val val 
        :class class 
        :generic gen
        :direction direction 
        :octave-offset octave-offset})))

;************* construct ********************

(defmulti interval b-types)

(defmethod interval 'Interval [i] i)

(defmethod interval :interval [n] 
  (let [[dn diroct] (dash-split n)
         class (degree (keyword dn))
         [dir oct] (dir-oct-expand diroct)
         gen (-> class :degree-class :val (* (:val dir)) (+ (* 7 oct)) generic-interval)
         val (* (:val dir) (+ (:val class) (* 12 oct)))]
    (build-interval n val dir oct class gen)))

(defmethod interval [:generic-interval :number] [gi n]
  (let [dc (-> (generic-interval-class gi) :name degree-class)
        [d-val v] [(:degree-val dc)(:val dc)]
        alt-n (let [x (- (mod12 n)(mod12 d-val))] 
                (cond (< x -2) (+ x 12) (> x 2) (- x 12) :else x))
        alt (alteration alt-n (:alt-type dc))
        dir-oct (second (dash-split gi))]
    (interval (keyword-cat (:name alt) (str (inc v)) "-" dir-oct))))

(defmethod interval 'Degree [d] (interval (keyword-cat (:name d) "-u")))
(defmethod interval :degree [d] (interval (keyword-cat d "-u")))

; (defmethod interval [:degree :direction :number] [d dir n] 
;   (interval (keyword-cat d "-" dir (str n))))

(defmethod interval ['Pitch 'Pitch] [p1 p2]
  (let [[p1v p2v] (map #(-> % :pitch-class :natural :val) [p1 p2])
         diff (- (:val p2) (:val p1))
         oct-diff (int-div diff 12)
         gicv (if (>= diff 0) (- p2v p1v) (-> (- p1v p2v) (+ 7) (mod 7) - ))
         gen (generic-interval (+ gicv (* 7 oct-diff)))]
    (interval (:name gen) (abs diff))))

(defmethod interval ['PitchClass 'PitchClass] [p1 p2]
  (interval (pitch p1) (pitch p2)))


;**********************************************

; (defmethod interval :generic-interval-class [g] 
;   (name->interval (keyword-cat (:name (generic->default-interval-class g)) :u)))

; (defmethod interval :number [v] (val->interval v))





