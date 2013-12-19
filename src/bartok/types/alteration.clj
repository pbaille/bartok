(in-ns 'bartok.types)

(def alterations
  (reduce #(conj %1 {:name (first %2) :val (second %2)})
          #{} {:# 1 :b -1 :x 2 :bb -2 nil 0}))

(def name->alteration (reduce #(into %1 {(:name %2) %2}) {} alterations))
(def val->alteration  (reduce #(into %1 {(:val %2) %2}) {} alterations))

(defrecord Alteration [name val])

(defn map->Alteration [m] (->Alteration (:name m) (:val m)))

(defmulti alteration
  (fn [arg]
    (cond
      (alteration-name? arg) :name
      (between arg [-2 2]) :val)))

(defmethod alteration :name [n] (map->Alteration (name->alteration n)))
(defmethod alteration :val [v] (map->Alteration (val->alteration v)))


