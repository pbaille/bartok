(in-ns 'bartok.litterals.all)

(load "types/generic_interval")

(def degree-classes
  (map #(with-type 'DegreeClass (zipmap [:alt-type :degree-val :val :name] [%4 %3 %2 %1]))
       [:root :second :third :fourth :fifth :sixth :seventh] 
       (range) 
       [0 2 4 5 7 9 11]
       [:t2 :t1 :t1 :t2 :t2 :t1 :t1]))

(def name->degree-class (reduce #(into %1 {(:name %2) %2}) {} degree-classes))
(def val->degree-class  (reduce #(into %1 {(:val %2) %2}) {} degree-classes))

(b-construct degree-class
  [:degree-class n] (name->degree-class n)
  [:generic-interval-class gic] (val->degree-class (-> gic generic-interval-class :val))
  [:number v] (val->degree-class v))