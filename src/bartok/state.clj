(ns bartok.state
  (:use utils.utils))

(def ^:dynamic *g* (atom {}))

(def ^:dynamic *settings* (atom
  {:tempo-interpolation :step ;# {:step :linear} TODO curve interpolations
}))