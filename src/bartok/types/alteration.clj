(in-ns 'bartok.types)

(def alterations
  (reduce #(conj %1 {:name (first %2) :val (second %2)})
          #{} {:# 1 :b -1 :x 2 :bb -2 nil 0}))

(def name->alteration (reduce #(into %1 {(:name %2) %2}) {} alterations))
(def val->alteration  (reduce #(into %1 {(:val %2) %2}) {} alterations))

(defmulti alteration b-types )

(defmethod alteration :alteration [n] 
  (with-type `Alteration (name->alteration n)))

(defmethod alteration :number [v] 
  (with-type `Alteration (val->alteration v)))


