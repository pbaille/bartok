(in-ns 'bartok.litterals.all)

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

(b-construct time-signature
  [:time-signature ts] (build-time-signature ts)
  [:number n :number d] (build-time-signature n d))


