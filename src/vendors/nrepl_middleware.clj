(ns vendors.nrepl-middleware)

(defn pprint-middleware [h]
  (fn [{:keys [code op] :as msg}]
    (if (and (= op "eval") (not (empty? code)))
      (->> #(str "(clojure.pprint/pprint " % ")")
           (update-in msg [:code])
           h)
      (h msg))))