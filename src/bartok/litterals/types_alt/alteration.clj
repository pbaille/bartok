(in-ns 'bartok.litterals.alt)

(defn- make-alterations-set [m]
  (reduce #(conj %1 (with-type 'Alteration (zipmap [:name :val] %2))) #{} m))

(def pitch-alterations    (make-alterations-set {:# 1 :b -1 :x 2 :bb -2 nil 0}))
(def degree-alterations-1 (make-alterations-set {:o -2 :m -1 :M 0 :# 1}))
(def degree-alterations-2 (make-alterations-set {:b -1 :P 0 :+ 1}))

(def alterations 
  (merge pitch-alterations 
         degree-alterations-1 
         degree-alterations-2))

(def name->alteration (reduce #(into %1 {(:name %2) %2}) {} alterations))
(def val->alteration  (reduce #(into %1 {(:val %2) %2}) {} pitch-alterations))

(b-construct alteration
  [:alteration n] (name->alteration n)
  [:number v] (val->alteration v)
  [:number v clojure.lang.Keyword t] 
    (cond (= t :t1) (select-first #(= v (:val %)) degree-alterations-1)
          (= t :t2) (select-first #(= v (:val %)) degree-alterations-2)
          (= t :pitch) (select-first #(= v (:val %)) pitch-alterations)))