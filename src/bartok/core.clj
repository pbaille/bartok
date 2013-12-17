(ns bartok.core
 (:use [midi])
 (:use [clojure.tools.namespace.repl :only (refresh)])
 (:use [bartok.midi-fns])
 (:use [bartok.RVal])
 (:use [utils.utils]))


(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main

  [& args]

  (def player (partial play-line vep ))
  (def line (map #(vector %1 (rand-int-between 40 90) %2)
                  (make-drunk-line [50 80] 7 50 60)
                  (map #(to-ms % 60) (r-line [1/2 1/3 1/4] 0))))
  (apply player line))

