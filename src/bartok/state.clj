(ns bartok.state
  (:use utils.utils)
  (:use bartok.midi.overtone-midi))

(def ^:dynamic *g* (atom {}))

(def ^:dynamic *midi-out* (atom 
  (midi-out "Gestionnaire IAC Bus IAC 2" )))

(def ^:dynamic *settings* (atom
  {:tempo-interpolation :step ;# {:step :linear} TODO curve interpolations
}))