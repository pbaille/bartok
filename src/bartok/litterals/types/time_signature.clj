(in-ns 'bartok.litterals.types)

(defn- parse-ts [ts]
  (->> (re-find #"([1-9][1-9]*)\|(2|4|8|16)" ts)
        next
       (map parse-int)))

(defn- build-time-signature
  ([named] (apply build-time-signature (parse-ts (name named)))) 
  ([num den]
    (with-type
      'TimeSignature
      {:name (keyword (str num "|" den))
       :val (* 4 (/ num den))
       :numerator num
       :denominator den})))

(defmulti time-signature b-types)

(defmethod time-signature :time-signature [ts] (build-time-signature ts))
(defmethod time-signature [:number :number] [n d] (build-time-signature n d))



