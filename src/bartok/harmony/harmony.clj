(ns bartok.harmony.harmony
  (:use [utils.utils])
  (:use [bartok.types])
  (:use [bartok.litterals.evaluation]))

(defn harmonic-domain 
  ([center] (harmonic-domain center center))
  ([center current]
     (let [center (b> center)
           current (b> current)
           h-funct (apply distance (map #(-> % mother-root pitch) [center current]))]
       (with-type 
         'HarmonicDomain
         {:center center
          :current current
          :h-function h-funct}))))  