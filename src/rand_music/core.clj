(ns rand-music.core
 (:use [midi])
 (:use [clojure.tools.namespace.repl :only (refresh)])
 (:use [rand-music.midi-fns])
 (:use [rand-music.RVal])
 (:use [utils.utils]))


(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(defn -main

  [& args]

  (def player (partial play-line vep ))
  (def line (map #(vector %1 (rand-int-between 40 90) %2)
                  (make-drunk-line [50 80] 7 50 600)
                  (map #(to-ms % 120) (r-line [1/2 1/3 1/4 1/6] 0))))
  (apply player line))


