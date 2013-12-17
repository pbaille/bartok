(ns utils.t-utils
  (:use midje.sweet)
  (:use [utils.utils]))

(facts "about keyword-cat"
  (fact "it concat two or more keywords"
    (keyword-cat :foo :bar) => :foobar
    (keyword-cat :foo :bar :aze) => :foobaraze))