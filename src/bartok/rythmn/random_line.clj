(ns bartok.rythmn.random-line
  (:use bartok.rythmn.rval)
  (:use bartok.structure.position)
  (:use utils.utils))

(defn- all-allowed-subs [rvals]
  (set (mapcat allowed-subs rvals)))

(defn- allowed-rvals [p rvals]
  (or (filter #(and (in? (all-allowed-subs rvals) (-> (pos+ p %) :sub denom))
                    (in? (allowed-subs %) (-> (pos+ p %) :sub denom)))
              (seq rvals))
      (println (str "no allowed rvals at" p \n 
                    "have to implement resolution"))))

(defn- rand-rval [p rvals]
  (rand-nth (allowed-rvals p rvals)))

(defn r-line 
  ([p rvals]
    (lazy-seq 
      (let [v (rand-rval p rvals)]
        (cons {:position p :duration v} 
              (r-line (pos+ p v) rvals)))))
  ([p rvals start-pos end-pos]
     (take-while #(< (pos-val (:position %)) 
                     (pos-val end-pos)) 
                 (r-line start-pos rvals))))

