(ns bartok.state
  (:use utils.utils)
  (:use bartok.midi.overtone-midi))

(def ^:dynamic *g* (atom {}))

(def ^:dynamic *m-out* 
  (midi-out "Gestionnaire IAC Bus IAC 2" ))

(def ^:dynamic *settings* (atom
  {:tempo-interpolation :linear ;# {:step :linear} TODO curve interpolations
}))