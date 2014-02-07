(ns bartok.melody.passing-tones
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use utils.all))

;Broderie => start on target pitch and return to it after several passing tones
;Passing => series of passing tones that target a pitch
  
; can be diatonic or chromatic

; have to specify a tonal context (a mode and a structure that contains target notes of the mode)

; types of passing-tones:
;- diatonic up and down (adjacents diatonic pitches)
;- chromatic up and down (adjacent chromatic pitches)
;- step up and down (adjacent structural pitches)

;on pitch can have several types, 
; if structure is [:M3 :+4 :M6 :M7] in Lydian mode
; :P5 can be the chromup/diatup of +4 and the diatdwn of :M6
; :+4 can be diatup/stepup of :M3 

; when computing possibilities, have to take care of those case, 
; different passing code can lead to same sequence, or sequence that should not contains repetitions can have some

; abreviations
; :cd => chromatic-down
; :dd => diatonic-down
; :sd => structure-down
; :cu => chromatic-up
; :du => diatonic-up
; :su => structure-up
; :me => target-note

(defn modal-struct 
  "takes an array of :c-interval-class(es) or CIntervalClass(es)
  return an array of CIntervalClass(es) with type 'ModalStruct"
  [cics]
  (with-type 'ModalStruct (b> cics)))

(b-fn degree-passing-tones 
  "return the passing environment of a degree"    
  [mc structur deg]
  (let [cu (b> :m2-u)
        cd (b> :m2-d)
        du (c-interval deg (diat-up mc deg) :u)
        dd (c-interval deg (diat-down mc deg) :d)
        su-cic (or (select-first #(> (:val %) (:val deg)) structur)
                   (first structur))
        su (c-interval deg su-cic :u)
        sd-cic (or (last (filter #(< (:val %) (:val deg)) structur))
                   (last structur))
        sd (c-interval deg sd-cic :d)]
    (array-map :degree deg :cu cu :cd cd :du du :dd dd :su su :sd sd )))

(b-fn passing-context 
  "assign to each degree of the mode-class its potential passings role"
  [mc structur]
  (map (p degree-passing-tones mc structur) (:degrees mc)))

(def passings
  "all possibles simples, doubles and triples passings series"
  (let [sple [[:cd] [:du] [:dd] [:su] [:sd]]
        dble (into [[:du :cu][:dd :cd][:du :cd][:cd :du]] ;chromatic passings ... maybe incomplete
                    (map vec (mapcat c/permutations 
                                    (c/combinations [:dd :du :sd :su] 2))))
        trple 
        (vec (mapcat 
               (fn [x] 
                 (keep #(when (not= %1 (first x)) 
                          (vec (cons %1 x))) 
                       [:dd :du :sd :su])) 
               dble))]
    {:simple sple :double dble :triple trple}))

(defn available-passings 
  "return a filtered subset of passings hash given a degree-passing-tones hash"
  [deg-pass-tones]
  (let [by-val (vals (group-by (f> val :val) deg-pass-tones))
        removed-uniqs (remove #(= 1 (count %)) by-val)
        overlaps (map (p map first) removed-uniqs)
        ;remove diatonics passings types because they occurs in every overlaps
        overlaps (map (p remove #(or= % :du :dd)) overlaps)]
    (reduce 
      (fn [acc el]
        (-> (update-in acc [:simple] 
              (p remove 
                (p some 
                  #(or (= % (first el)) 
                       (= % (second el))))))
            (update-in [:double]
              (p remove 
                (p some 
                  #(or (= % (first el)) 
                       (= % (second el))))))
            (update-in [:triple]
              (p remove 
                (p some 
                  #(or (= % (first el)) 
                       (= % (second el))))))))
      passings 
      overlaps)))

(def- int->passing-size {1 :simple 2 :double 3 :triple})

(defn make-passing-serie 
  "returns a passing-serie
  sizes can be chain for composed passing-series
  ex: (make-passing-serie 
        (last (passing-context :Lyd [:M3 :+4 :+5 :M7])) 
        true ;it is a broderie
        2 2)"
  [deg-pass-tones broderie? & sizes]
  (let [av-pass (available-passings deg-pass-tones)]
    (reduce 
      (fn [acc siz]
        (into acc (conj (rand-nth (siz av-pass)) :me))) 
      (if broderie? [:me] []) ;start on target note if broderie 
      (map int->passing-size sizes))))


