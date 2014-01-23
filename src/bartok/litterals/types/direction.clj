(in-ns 'bartok.litterals.all)

(def directions 
   {:up (with-type 'Direction {:name :u :val 1})
    :down (with-type 'Direction {:name :d :val -1})})

(defmulti direction b-types)

(defmethod direction :direction [n] 
  (if (= n :u) (directions :up) 
               (directions :down)))

(defmethod direction :number [v] 
  (if (= v 1) (directions :up) 
              (directions :down)))
