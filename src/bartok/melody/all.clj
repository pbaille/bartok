(ns bartok.melody.all
  (:use utils.utils)
  (:use bartok.litterals.all))

(immigrate
  'bartok.melody.melodic-domain
  'bartok.melody.step-pattern
  'bartok.melody.strategies
  'bartok.melody.analysis)
