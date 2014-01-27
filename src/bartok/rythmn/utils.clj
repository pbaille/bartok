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