(ns bartok.rythmn.utils
  (:use bartok.rythmn.rval)
  (:use bartok.structure)
  (:use utils.utils))

(defn all-allowed-subs [rvals]
  (set (mapcat allowed-subs rvals)))

(defn allowed-rvals [p rvals]
  (or (filter #(and (in? (all-allowed-subs rvals) (-> (pos+ p %) :sub denom))
                    (in? (allowed-subs %) (-> (pos+ p %) :sub denom)))
              (seq rvals))
      (println (str "no allowed rvals at" p \n 
                    "have to implement resolution"))))

(defn timable-queue
  "
  assign position to each timable
  a timable is any map that contains a duration field 
  (or a number that will be map to {:duration number})
  
  ex: 
  (timable-queue [{:duration 3/2}{:duration 3/4}])
  => ({:position {:cycle 0, :bar 0, :sub 0}, :duration 3/2} 
      {:position {:cycle 0, :bar 0, :sub 3/2}, :duration 3/4})
  "
  ([durations] (timable-queue (g-pos) durations))
  ([pos durations]
    (let [durations (if (number? (first durations)) 
                      (map #(hash-map :duration %) durations) 
                      durations)]
      (letfn [(fun [pos [fd & nd]]
                (if-not (seq nd)
                  (list (assoc fd :position pos))
                  (cons (assoc fd :position pos)
                        (fun (pos+ pos (:duration fd)) nd))))]
        (fun pos durations)))))
