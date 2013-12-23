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

(defmulti interval b-types)

(defmethod interval :interval [n] 
  (let [[dn diroct] (dash-split n)
         class (degree (keyword dn))
         [dir oct] (dir-oct-expand diroct)
         gen (-> class :degree-class :val (* (:val dir)) (+ (* 7 oct)) generic-interval)
         val (* (:val dir) (+ (:val class) (* 12 oct)))]
    (build-interval n val dir oct class gen)))

(defmethod interval 'Degree [d] (interval (keyword-cat (:name d) "-u")))
(defmethod interval :degree [ic] (interval (keyword-cat ic "-u")))

; (defmethod interval :generic-interval-class [g] 
;   (name->interval (keyword-cat (:name (generic->default-interval-class g)) :u)))

; (defmethod interval :number [v] (val->interval v))





