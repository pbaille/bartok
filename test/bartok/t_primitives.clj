(ns bartok.t-primitives
  (:use midje.sweet)
  (:use bartok.primitives))

(fact "direction"
  (direction :u) => {:name :u, :val 1}
  (direction 1) => {:name :u, :val 1})

(fact "alteration"
  (alteration :#) => {:val 1, :name :#}
  (alteration 1) => {:val 1, :name :#}
  (alteration 1 :t2) => {:val 1, :name :+}
  (alteration -2 :t1) => {:val -2, :name :o}
  (alteration 2 :pitch) => {:val 2, :name :x})

(facts "d-interval-class"
  (fact "construct"
    (d-interval-class :7th) => {:name :7th, :val 6, :degree-val 11, :alt-type :t1}
    (d-interval-class 1) => {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}
    (d-interval-class (d-interval :7th-u)) => {:name :7th, :val 6, :degree-val 11, :alt-type :t1})
  (fact "arithmetics"
    (b:+ :7th :2nd) => {:name :1st, :val 0, :degree-val 0, :alt-type :t2}
    (b:+ :5th :2nd) => {:name :6th, :val 5, :degree-val 9, :alt-type :t1}
    (b:- :2nd :2nd) => {:name :1st, :val 0, :degree-val 0, :alt-type :t2}
    (b:- :7th :2nd) => {:name :6th, :val 5, :degree-val 9, :alt-type :t1}))

(facts "d-interval"
  (fact "construct"
    (d-interval :7th-u) 
    => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
    (d-interval 6) 
    => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
    (d-interval (d-interval-class :7th)) 
    => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
    (d-interval :7th :d) 
    => {:name :7th-d, :val -6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :d, :val -1}, :octave-offset 0})  
  (fact "arithmetics"
    (b:+ :7th-u :2nd-u) => (d-interval :1st-u1)
    (b:+ :5th-u1 :2nd-d) => (d-interval :4th-u1)
    (b:- :2nd-u1 :2nd-d) => (d-interval :3rd-u1)
    (b:- :7th-d2 :2nd-u) => (d-interval :1st-d3)))

(facts "c-interval-class"
  (fact "construct"
    (c-interval-class :m2) => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
    (c-interval-class 1) => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
    (c-interval-class :2nd) => {:name :M2, :val 2, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
    (c-interval-class  :Bb :C#) => (c-interval-class :#2)   
    (c-interval-class  :C# :Bb) => (c-interval-class :o7)
    (c-interval-class (c-interval :m2-u1))
      => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}} 
    (c-interval-class :C-Dor) => {:name :M6, :val 9, :d-class {:name :6th, :val 5, :degree-val 9, :alt-type :t1}}
    (c-interval-class :Dor) => {:name :M6, :val 9, :d-class {:name :6th, :val 5, :degree-val 9, :alt-type :t1}})
  (fact "arithmetics"
    (b:+ :m7 :M2) => (c-interval-class :P1)
    (b:+ :m7 :M2) => (c-interval-class :P1)
    (b:- :m7 :M2) => (c-interval-class :m6)
    (b:- :m2 :P4) => (c-interval-class :m6)))

(facts "c-interval"
  (fact "construct"
    (c-interval :m2-u1)
    => {:name :m2-u1, :val 13, :class (c-interval-class :m2) :diatonic (d-interval :2nd-u1), :direction {:name :u, :val 1}, :octave-offset 1}
    (c-interval :2nd-u1)
    => {:name :M2-u1, :val 14, :class (c-interval-class :M2) :diatonic (d-interval :2nd-u1), :direction {:name :u, :val 1}, :octave-offset 1}
    (c-interval :m2)
    => {:name :m2-u, :val 1, :class (c-interval-class :m2) :diatonic (d-interval :2nd-u), :direction {:name :u, :val 1}, :octave-offset 0}
    (c-interval :2nd (- 13)) => (c-interval :m2-d1)
    (c-interval :2nd 3) => (c-interval :#2-u)
    (c-interval :B :A#) => (c-interval :m2-d) 
    (c-interval 'E 'C) => (c-interval :M3-d)
    (c-interval :C2 :B1) => (c-interval :m2-d)
    (c-interval :M3 :P4 :u) => (c-interval :m2-u)
    (c-interval :M3 :P4 :d) => (c-interval :M7-d)
    (c-interval :P4 :M3 :u) => (c-interval :M7-u)
    (c-interval :P4 :M3 :d) => (c-interval :m2-d))
  (fact "arithmetics"
    (b:+ :m2-u :M7-u) => (c-interval :P1-u1)
    (b:+ :m2-u :M7-d) => (c-interval :#6-d)
    (b:- :m2-u :M7-d) => (c-interval :P1-u1)
    (b:- :m2-u :M7-u) => (c-interval :#6-d))
  
  (fact "invert"
    (invert :m2-d) => (c-interval :m2-u)
    (invert :m2-u) => (c-interval :m2-d))
  ; ;['Pitch p1 'Pitch p2]
)

(fact "natural-pitch-class"
  (natural-pitch-class 'A) => {:name 'A, :val 5, :pitch-val 9}
  (natural-pitch-class 5) => {:name 'A, :val 5, :pitch-val 9}
  (transpose 'A 1) => {:name 'B, :val 6, :pitch-val 11}
  (transpose 'A :2nd-u2) => {:name 'B, :val 6, :pitch-val 11}
  
  (d-interval-class 'A 'B) => {:name :2nd, :val 1, :degree-val 2, :alt-type :t1})

(fact "pitch-class"
  (pitch-class  2) => {:name :D, :val 2, :natural {:name 'D, :val 1, :pitch-val 2}, :alteration {:val 0, :name nil}} 
  (pitch-class :D) => {:name :D, :val 2, :natural {:name 'D, :val 1, :pitch-val 2}, :alteration {:val 0, :name nil}} 
  (pitch-class 'A) => {:name :A, :val 9, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 0, :name nil}}
  (pitch-class 'A 10) => {:name :A#, :val 10, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 1, :name :#}} 
  (pitch-class 'A :#) => {:name :A#, :val 10, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 1, :name :#}} 
  (pitch-class :C#1) => (pitch-class :C#)
  (transpose :C# :m2-d) => (pitch-class :B#))

(fact "pitch"
  (pitch 60) => {:name :C0, :val 60, :octave 0, :pitch-class (pitch-class :C)}
  (pitch :C0) => {:name :C0, :val 60, :octave 0, :pitch-class (pitch-class :C)}
  (pitch :C) => {:name :C0, :val 60, :octave 0, :pitch-class (pitch-class :C)}
  (pitch 'C 61) => {:name :C#0, :val 61, :octave 0, :pitch-class (pitch-class :C#)}
  (pitch 'C :# 0) => {:name :C#0, :val 61, :octave 0, :pitch-class (pitch-class :C#)}
  ; (mode 'C-Lyd 2)
  (distance :C#1 :B2) => 22
  (is-alteration-of :C1 :Cb1) => true
  (highest :C1 :B2) => (pitch :B2)
  (transpose :C1 :m2-u1) => (pitch :Db2)
  (transpose :C1 :m7-d) => (pitch :D0)
  (in-mode? :C# :C-Lyd) => nil
  (in-mode? :C :C-Lyd) => true)

(fact "mode-class"
  (mode-class :Mix) => (mode-class :Lyd 2))

(fact "mode"
  (mode :C-Dor) => (mode :Eb-Lyd 6)
  (mode :A-Loc2) => (transpose :G-Loc2 :M2-u)
  (mother-root (mode :B-Loc2)) => (pitch-class :F)
  (mother-mode (mode :B-Loc2)) => (mode :F-Lyd+)
  (relative (mode :D-Dor) :Lyd) => (mode :F-Lyd)
  (intra-abs-move (mode :D-Dor) 3) => (mode :A-Eol)
  (intra-rel-move (mode :D-Dor) 4) => (mode :A-Eol)
  (transpose :B-Dor+4 :M2-u1) => (mode :C#-Dor+4)
  )

(fact "time-signature"
  (time-signature :4|4) => {:name :4|4, :val 4, :numerator 4, :denominator 4}
  (time-signature 4 4) => {:name :4|4, :val 4, :numerator 4, :denominator 4})
