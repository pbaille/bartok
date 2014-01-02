(ns bartok.rythmn.random-line
  (:use bartok.rythmn.rval)
  (:use bartok.structure.grid)
  (:use utils.utils))

(defn- all-allowed-subs [rvals]
  (set (mapcat allowed-subs rvals)))

(defn- allowed-rvals [grid rvals]
  (or (filter #(in? (all-allowed-subs rvals) 
                    (-> (position-add grid %) :position :sub denom))
              (seq rvals))
      (println (str "no allowed rvals at" (:position grid) \n 
                    "have to implement resolution"))))

(defn- rand-rval [grid rvals]
  (rand-nth (allowed-rvals grid rvals)))

(defn r-line
  [grid rvals]
   (lazy-seq
     (let [v (rand-rval grid rvals)]
       (cons v (r-line (position-add grid v) rvals)))))
