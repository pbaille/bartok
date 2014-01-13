(ns bartok.rythmn.random-line
  (:use bartok.rythmn.utils)
  (:use bartok.structure.position)
  (:use utils.utils))

(defn- rand-rval [p rvals]
  (rand-nth (allowed-rvals p rvals)))

(defn r-line 
  ([p rvals]
    (lazy-seq 
      (let [v (rand-rval p rvals)]
        (cons {:position p :duration v} 
              (r-line (pos+ p v) rvals)))))
  ([rvals start-pos end-pos]
     (take-while #(< (pos-val (:position %)) 
                     (pos-val end-pos)) 
                 (r-line start-pos rvals))))

