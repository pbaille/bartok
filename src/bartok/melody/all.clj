(ns bartok.melody.all
  (:use utils.utils)
  (:use bartok.primitives))

(immigrate
  'bartok.melody.melodic-domain
  'bartok.melody.step-pattern
  'bartok.melody.strategies
  'bartok.melody.analysis)
