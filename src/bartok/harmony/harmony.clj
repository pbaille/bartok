(ns bartok.harmony.harmony
  (:use [utils.utils])
  (:use [bartok.types])
  (:use [bartok.harmony.modal-move])
  (:use [bartok.litterals.evaluation]))

(defn harmonic-domain 
  ([current] (harmonic-domain (mother-mode (b> current)) current))
  ([center current]
     (let [center (b> center)
           current (b> current)
           h-funct (apply interval (map #(-> % mother-root pitch) [center current]))]
       (with-type 
         'HarmonicDomain
         {:center center
          :current current
          :h-function (modal-move h-funct)}))))  