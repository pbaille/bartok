(ns z.contracts-try
  (:use clojure.core.contracts))

(def doubler 
  (contract doubler
  "ensures doubling"
  [x] [number? => (= (* 2 x) %)]
  [x y] [(every? number? [x y])
           =>
         (= (* 2 (+ x y)) %)]))

(def secure-doubler
  (with-constraints
    (fn 
      ([a] (* 2 a))
      ([a b] (* 2 (+ a b))))
    doubler))

