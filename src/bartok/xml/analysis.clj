(ns bartok.xml.analysis
  (:use utils.all)
  (:use bartok.primitives)
  (:use bartok.types.note)
  (:use bartok.melody.all)
  (:use [bartok.xml parser transform])
  (:use bartok.structure))

(defn- harmonic-an [m]
  (closest-mothers3 (measure-notes m)))