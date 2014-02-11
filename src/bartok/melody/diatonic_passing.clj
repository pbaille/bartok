(ns bartok.melody.diatonic-passing
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use bartok.types.w-mode)
  (:use utils.all))

(b-fn x-d-passing-context
  "return the passing environment of a degree/pitch-class in a w-mode(-class)"    
  [wm?c deg-pc]
  [(:name deg-pc)
   {:chromatic-up   (chromatic-up wm?c deg-pc)
    :chromatic-down (chromatic-down wm?c deg-pc)
    :auxs-up        (auxs-up wm?c deg-pc)
    :auxs-down      (auxs-down wm?c deg-pc)
    :main-up        (main-up wm?c deg-pc)
    :main-down      (main-down wm?c deg-pc)}])

(b-multi d-passing-context
  "assign to each degree of a w-mode(-class) its passing environment")

(b-meth d-passing-context 'WMode [wm] 
  (entries->h-map (map (p x-d-passing-context wm) (:pitch-classes wm))))

(b-meth d-passing-context 'WModeClass [wmc] 
  (entries->h-map (map (p x-d-passing-context wmc) (:degrees wmc))))
