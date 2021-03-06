(ns bartok.primitives
  (:use utils.all)
  (:use midje.sweet)
  (:require [camel-snake-kebab :as csk])
  (:use vendors.debug-repl))

(load-file "src/bartok/litterals.clj")
(load-file "src/bartok/types.clj")
  
  ; (declare b-types)
  
  ; (defn b-type [x]
  ;   (cond
  ;     (named? x) (or (b? x) (type x))
  ;     (number? x) (if (ratio? x) :ratio :number)
  ;     (type= x clojure.lang.PersistentVector) 
  ;       (let [[ft :as bts] (a b-types x)] 
  ;         (if (all-eq? bts) [ft] bts))
  ;     (type= x clojure.lang.PersistentHashSet) (set (a b-types x))
  ;     (type= x clojure.lang.PersistentList) (a b-types x)
  ;     (type= x clojure.lang.PersistentArrayMap) 'Map ;(map-h* b-type x)
  ;     (type= x clojure.lang.PersistentHashMap)  'Map ;(map-h* b-type x)
  ;     :else (type x)))
  
  ; (defn b-types 
  ;   "ex: 
  ;   (b-types {:m2 'A :Lyd :4|4} (b> :m2) #{:C# :C#2})
  ;   => [{:mode-class :time-signature, :c-interval-class :natural-pitch-class} 
  ;       CIntervalClass 
  ;       #{:pitch-class :pitch}]"
  ;   [& xs] (mapv b-type xs))
  
;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------
  
  (declare comp-b>)
  
  (defn b> 
    ([x] 
     (let [t (b-type x)] 
       (cond 
         (or (= t 'Number)(= t 'Ratio)) x
         (litteral-type? t) (call (name t) x) 
         (fn? x) (comp-b> x)
         (type= x clojure.lang.PersistentVector)  (mapv b> x)
         (type= x clojure.lang.PersistentHashSet) (set (map b> x))
         (type= x clojure.lang.PersistentArrayMap) (map-h* b> x)
         (type= x clojure.lang.PersistentHashMap)  (map-h* b> x)
         (and (not (map? x)) (sequential? x)) (mapv b> x)
         :else x)))
    ([x & xs] (map b> (cons x xs))))
  
  (defn comp-b> [f]
    #(apply f (map b> %&)))
  
  (defn b>> [f & args]
    (apply (comp-b> f) args))
  
  (defmacro b-fn [n args & body]
    `(do (defn ~n ~args ~@body)
         (def  ~n (comp-b> ~n))))
  
  (defmacro b-def [n x] 
    `(def ~n (b> ~x)))
  
  (defmacro b-multi [n & body]
    (let [doc-string (when (string? (first body)) (first body))
          body (if doc-string (next body) body)]
     `(do (defmulti ~n b-types)
        (defmethod ~n :default [& args#] 
          (let [b-args# (b> args#)
                disp-vals# (a b-types b-args#)] 
            (if (contains? (methods ~n) disp-vals#) 
              (a ~n b-args#)
              (throw (Exception. 
                (str "*** No dispatch value " 
                     disp-vals# 
                     " for bartok-multimethod " 
                     (name '~n) " ***"))))))
        ~@(map (fn [met] `(defmethod ~n ~@met)) body))))
  
  (defmacro b-construct [n & body]
    `(do (b-multi ~n)
       (defmethod ~n ['~(csk/->CamelCase (symbol (name n)))] [x#] x#)
       ~@(map (fn [[v & fun-body]]
                 (let [types (vec (take-nth 2 v))
                       args  (vec (take-nth 2 (next v)))]
                   `(defmethod ~n ~types ~args ~@fun-body))) 
              (partition 2 2 body))))
  ;for readibility of b-multi methods
  (defmacro b-meth [& args] `(defmethod ~@args))
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; multi-methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  (b-multi to-num)
  (b-meth  to-num :default [x] 
    {:pre [(or (of-type? x Number) (:val (b> x)))]} 
    (if (of-type? x Number)
      x
      (:val (b> x))))
  
  (b-multi transpose)
  (b-multi invert)
  
  ;modal-moves
  (b-multi intra-abs-move)
  (b-multi intra-rel-move)
  (b-multi relative)
  
  ;arithmetics
  (b-multi b:+)
  (b-multi b:-)
  
  ;intra-mode degree-move
  ;return the nth diatonic pitchclass or degree of 2nd argument 
  ;in the given direction (up if (pos? n) or down otherwise)"
  (b-multi nth-diat)
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;; generic methods ;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  ;passing
  (defn chrom-up [x] (transpose x :m2-u))
  (defn chrom-down [x] (transpose x :m2-d))
  
  ;comparators
  
  (b-fn b:= [x y] 
    {:pre [(same-type? x y)]}
    (= x y))
  
  (b-fn b:> [x y] 
    {:pre [(same-type? x y)]}
    (> (:val x) (:val y)))
  
  (b-fn b:< [x y] 
    {:pre [(same-type? x y)]}
    (< (:val x) (:val y)))
    
  (b-fn b:>= [x y] 
    {:pre [(same-type? x y)]}
    (>= (:val x) (:val y)))
      
  (b-fn b:<= [x y] 
    {:pre [(same-type? x y)]}
    (<= (:val x) (:val y)))
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  ;;;;;;;;;;;;;;;;;;;; Direction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def directions 
       {:up   (with-type 'Direction {:name :u :val 1})
        :down (with-type 'Direction {:name :d :val -1})})
    
    (b-construct direction
      ['direction d]
        (if (= d :u) (directions :up) 
                     (directions :down))
      ['Number n]
        (if (= n 1) (directions :up) 
                    (directions :down)))

  ;;;;;;;;;;;;;;;;;;;; Alteration ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (defn- make-alterations-set [m]
      (reduce #(conj %1 (with-type 'Alteration (zipmap [:name :val] %2))) #{} m))
    
    (def pitch-alterations    (make-alterations-set {:# 1 :b -1 :x 2 :bb -2 nil 0}))
    (def degree-alterations-1 (make-alterations-set {:o -2 :m -1 :M 0 :# 1}))
    (def degree-alterations-2 (make-alterations-set {:b -1 :P 0 :+ 1}))
    
    (def alterations 
      (concat pitch-alterations 
              degree-alterations-1 
              degree-alterations-2))
    
    (def name->alteration (reduce #(into %1 {(:name %2) %2}) {} alterations))
    (def val->alteration  (reduce #(into %1 {(:val %2) %2}) {} pitch-alterations))
    
    (b-construct alteration
      ['alteration n] (name->alteration n)
      ['Number v] (val->alteration v)
      ['Number v 'Keyword t] 
        (cond (= t :t1) (select-first #(= v (:val %)) degree-alterations-1)
              (= t :t2) (select-first #(= v (:val %)) degree-alterations-2)
              (= t :pitch) (select-first #(= v (:val %)) pitch-alterations)))
    
  ;;;;;;;;;;;;;;;;;; DIntervalClass ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def d-interval-classes 
      (map #(with-type 'DIntervalClass (zipmap [:alt-type :degree-val :val :name] [%4 %3 %2 %1]))
           [:1st :2nd :3rd :4th :5th :6th :7th] 
           (range) 
           [0 2 4 5 7 9 11]
           [:t2 :t1 :t1 :t2 :t2 :t1 :t1]))
    
    (def name->d-interval-class (reduce #(into %1 {(:name %2) %2}) {} d-interval-classes))
    (def val->d-interval-class  (reduce #(into %1 {(:val %2) %2}) {} d-interval-classes))
    
    (defn- dir-oct-expand [x] 
      (if-let [[[_ dir oct]] (re-seq #"([ud])([0-9])*" (name x))]
        [(direction (keyword dir))
         (if oct (parse-int oct) 0)]))
    
    ;; Construct ;;;
    
    (b-construct d-interval-class
                 
      ['d-interval-class n] 
        (name->d-interval-class n)
        
      ['DInterval gi] (:class gi)
        
      ['Number v] 
        (val->d-interval-class v)
        
      ['NaturalPitchClass npc1 'NaturalPitchClass npc2]
        (let [dist (mod (- (:val npc2)(:val npc1)) 7)]
          (d-interval-class dist)))
    
    ;;; arithmetics ;;;
    
    (b-meth b:+ ['DIntervalClass 'DIntervalClass] [dic1 dic2]
      (d-interval-class (mod (+ (:val dic1) (:val dic2)) 7)))
    
    (b-meth b:- ['DIntervalClass 'DIntervalClass] [dic1 dic2]
      (d-interval-class (mod (- (:val dic1) (:val dic2)) 7)))
    
  ;;;;;;;;;;;;;;;;;;;; DInterval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; Construct ;;;
    
    (b-construct d-interval 
                 
      ['d-interval n] 
        (let [[gin diroct] (dash-split n)
               class (d-interval-class (keyword gin))
               [dir oct] (dir-oct-expand diroct)
               val (* (:val dir) (+ (:val class) (* 7 oct)))]
          (with-type 'DInterval 
                     {:name n :val val :class class :direction dir :octave-offset oct}))
        
      ['Number v]
        (let [[oct m] (div-mod (abs v) 7)
              class (d-interval-class m)
              dir (direction (if (>= v 0) :u :d))
              n (kwcat (:name class) "-" (:name dir) (if (= 0 oct) "" (str oct)))]
          (with-type 'DInterval 
                     {:name n :val v :class class :direction dir :octave-offset oct}))
        
      ['DIntervalClass n]
        (d-interval (kwcat (:name n) "-u"))
      
      ['DIntervalClass gic 'Direction d]
        (d-interval (kwcat (:name gic) "-" (:name d)))
        
      ['CInterval ci] (:diatonic ci))
    
    ;;; arithmetics ;;;
    
    (b-meth b:+ ['DInterval 'DInterval] [dic1 dic2]
      (d-interval (+ (:val dic1) (:val dic2))))
    
    (b-meth b:- ['DInterval 'DInterval] [dic1 dic2]
      (d-interval (- (:val dic1) (:val dic2))))
    
  ;;;;;;;;;;;;;;;;;; CIntervalClass ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (declare c-interval-class)
    
    (def c-interval-classes
        (for [{cn :name cv :val ddv :degree-val ct :alt-type :as dc} d-interval-classes
              alt (cond (= ct :t1) degree-alterations-1
                        (= ct :t2) degree-alterations-2)]
          (let [n (kwcat (:name alt) (-> dc :val inc str))
                v (mod12 (+ ddv (:val alt) 12))] 
            (with-type 'CIntervalClass {:name n :val v :d-class dc}))))
    
    (def d-interval-class->c-interval-class
      (reduce #(into %1 {(-> %2 :d-class :name) %2}) {} 
              (filter #(#{:P1 :M2 :M3 :P4 :P5 :M6 :m7} (:name %)) c-interval-classes)))
    
    (def c-interval-class-default-names 
      #{:P1 :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})
    
    (def name->c-interval-class (reduce #(into %1 {(:name %2) %2}) {} c-interval-classes))
    
    (def val->c-interval-class  
      (reduce #(into %1 {(:val %2) %2}) {} 
              (filter #(c-interval-class-default-names (:name %)) c-interval-classes)))
    
    ;;; construct ;;;
    
    (b-construct c-interval-class
      ['c-interval-class n] (name->c-interval-class n)
      ['Number v] (val->c-interval-class (mod12 v))
      ['DIntervalClass dc] (d-interval-class->c-interval-class (:name dc))
      ['Mode m] (-> m :mode-class :degree)
      ['ModeClass m] (:degree m)
      ['CInterval m] (:class m)
      
      ['DIntervalClass di 'Number n]
        (let [dv (:degree-val di)
              alt (alteration (- n dv) (:alt-type di))]
          (when alt (c-interval-class (kwcat (:name alt) (inc (:val di))))))
      
      ['DIntervalClass dic 'Alteration alt]  
        (let [alt (alteration (alt :val) (-> dic :alt-type))]
          (name->c-interval-class (kwcat (alt :name) (inc (dic :val)))))
        
      ['PitchClass pc1 'PitchClass pc2]
        (let [dic (d-interval-class (:natural pc1) (:natural pc2))
              diff (mod12 (- (:val pc2)(:val pc1)))]
          (c-interval-class dic diff)))
    
    ;;; Arithmetics ;;;
    
    (b-meth b:+ ['CIntervalClass 'CIntervalClass] [cic1 cic2]
      (c-interval-class (b:+ (:d-class cic1) (:d-class cic2))
                  (mod12 (+ (:val cic1) (:val cic2)))))
    
    (b-meth b:- ['CIntervalClass 'CIntervalClass] [cic1 cic2]
      (c-interval-class (b:- (:d-class cic1) (:d-class cic2))
                  (mod12 (- (:val cic1) (:val cic2)))))
    
    ;;; functions ;;;
    
    (b-meth relative ['CIntervalClass] [d] 
      (c-interval-class (- 12 (:val d))))
    
  ;;;;;;;;;;;;;;;;;;;; CInterval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (declare pitch)
    
    (defn build-c-interval 
      ([m]
         (let [{:keys [name val direction octave-offset class diatonic]} m]
           (build-c-interval name val direction octave-offset class diatonic)))
      ([name val direction octave-offset class diat]
         (with-type 
           'CInterval 
           {:name name 
            :val val 
            :class class 
            :diatonic diat
            :direction direction 
            :octave-offset octave-offset})))
    
    ;;; construct ;;;
    
    (b-construct c-interval 
      ['c-interval n] 
        (let [[dn diroct] (dash-split n)
               class (c-interval-class (keyword dn))
               [dir oct] (dir-oct-expand diroct)
               gen (-> class :d-class :val (+ (* 7 oct)) (* (:val dir)) d-interval)
               val (* (:val dir) (+ (:val class) (* 12 oct)))]
          (build-c-interval n val dir oct class gen))
      
      ['DInterval di]
        (c-interval (kwcat (-> di :class c-interval-class :name)
                          "-"
                          (-> di :direction :name)
                          (when-not (zero? (:octave-offset di)) 
                            (:octave-offset di)))) 
      
      ['CIntervalClass d] (c-interval (kwcat (:name d) "-u"))
      
      ['CIntervalClass ci 'Direction d] 
        (c-interval (kwcat (:name ci) "-" (:name d)))
      
      ['CIntervalClass d 'Number oct-offset]
        (c-interval (kwcat (:name d) 
                           (if (>= oct-offset 0) "-u" "-d") 
                           (when-not (zero? oct-offset) (abs oct-offset))))
      
      ['DIntervalClass di 'Number n]
        (let [[d-val v] [(:degree-val di)(:val di)]
              alt-n (let [x (- (mod12 (abs n)) d-val)] 
                      (cond (< x -2) (+ x 12) (> x 2) (- x 12) :else x))
              alt (alteration alt-n (:alt-type di))
              dir (if (>= n 0) :u :d)
              oct (when-not (zero? (int-div n 12)) (abs (int-div n 12)))]
          (or-dr (c-interval (kwcat (:name alt) (str (inc v)) "-" dir oct))))
      
      ['DInterval di 'Number n] (c-interval (:class di) n)
      
      ['NaturalPitchClass npc1 'NaturalPitchClass npc2]
        (let [dist (- (:val npc2)(:val npc1))
              di (d-interval dist)]
          (c-interval di))
      
      ['PitchClass p1 'PitchClass p2]
        (let [[npc1 npc2] (map :natural [p1 p2])
               npc-dist (- (:val npc2)(:val npc1))
               dic (:class (d-interval npc-dist))
               dist (- (:val p2)(:val p1))]
          (c-interval dic dist))
      
      ['Pitch p1 'Pitch p2]
        (let [[pc1 pc2] (map :pitch-class [p1 p2])
               pci (c-interval pc1 pc2)
               oct-inter (c-interval (c-interval-class :P1) 
                                     (- (:octave p2)(:octave p1)))]
          (b:+ pci oct-inter))
      
      ['PitchClass pc1 'PitchClass pc2 'Direction d]
        (let [ci (c-interval pc1 pc2)]
          (if (= (:direction ci) d) ci (relative ci)))  
        
      ['CIntervalClass ci1 'CIntervalClass ci2 'Direction d]
        (let [up? (= d (direction :u))
              chrom-dist (if up? 
                           (mod12 (- (-> ci2 :val)(-> ci1 :val)))
                           (- (mod12 (- (-> ci1 :val)(-> ci2 :val)))))
              diat-dist (if up? 
                          (mod (- (-> ci2 :d-class :val)(-> ci1 :d-class :val)) 7)
                          (mod (- (-> ci1 :d-class :val)(-> ci2 :d-class :val)) 7))]
          (c-interval (d-interval-class diat-dist) chrom-dist)))
    
    ;;; Arithmetics ;;;
    
    (b-meth b:+ ['CInterval 'CInterval] [ci1 ci2]
      (c-interval (b:+ (d-interval ci1) (d-interval ci2))
                  (+ (:val ci1) (:val ci2))))
    
    (b-meth b:- ['CInterval 'CInterval] [ci1 ci2]
      (c-interval (b:- (d-interval ci1) (d-interval ci2))
                  (- (:val ci1) (:val ci2))))
    
    ;;;
    
    (b-meth invert ['CInterval] [ci]
      (c-interval (-> ci :class :d-class) (- (:val ci))))
    
    (b-meth relative ['CInterval] [ci]
      (invert (c-interval (relative (:class ci)) 
                          (if (zero? (:octave-offset ci)) 
                            (:direction ci) 
                            (:octave-offset ci)))))
  
  ;;;;;;;;;;;;;;;;; NaturalPitchClass ;;;;;;;;;;;;;;;;;;;;;;;
    
    (def natural-pitch-classes
      (map #(with-type 'NaturalPitchClass (zipmap [:pitch-val :val :name] [%3 %2 %1]))
           ['C 'D 'E 'F 'G 'A 'B] (range) [0 2 4 5 7 9 11]))
    
    (def name->natural-pitch-class (reduce #(into %1 {(:name %2) %2}) {} natural-pitch-classes))
    (def val->natural-pitch-class  (reduce #(into %1 {(:val %2) %2}) {} natural-pitch-classes))
    
    ;;; construct ;;;
    
    (b-construct natural-pitch-class 
      ['natural-pitch-class n] (name->natural-pitch-class n)
      ['Number v] (val->natural-pitch-class (mod v 7)))
    
    ;;; methods ;;;
    
    (b-meth transpose ['NaturalPitchClass 'DInterval] [this gi]
      (natural-pitch-class (+ (:val this) (:val gi))))
    
    (b-meth transpose ['NaturalPitchClass 'Number] [this n]
      (natural-pitch-class (+ (:val this) n)))
    
  ;;;;;;;;;;;;;;;;;;;; PitchClass ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def pitch-classes 
      (reduce conj #{}      
        (for [{npcn :name npcv :pitch-val} natural-pitch-classes 
              {an :name av :val} pitch-alterations]
          (with-type 
            'PitchClass
            {:name (if (= av 0) (keyword npcn) (kwcat npcn an))
             :val (mod (+ npcv av 12) 12)
             :natural (natural-pitch-class npcn)
             :alteration (alteration av)}))))
    
    (def pitch-class-defaults-names 
      #{:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B})
    
    (def default-name-pitch-classes 
      (filter #(in? pitch-class-defaults-names (:name %)) pitch-classes))
    
    ; for performance
    (def name->pitch-class (reduce #(into %1 {(:name %2) %2}) {} pitch-classes))
    (def val->pitch-class  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitch-classes))
    
    ;;; Constructor ;;;
    
    (b-construct pitch-class 
      ['Number v] 
        (val->pitch-class v)
      ['pitch-class n] 
        (name->pitch-class n)
      ['NaturalPitchClass n] 
        (name->pitch-class (keyword (:name n)))
      ['Pitch p] 
        (:pitch-class p)
      ['NaturalPitchClass n 'Number v] 
        (select-first #(and (= (get-in % [:natural :name]) (:name n))
                            (= (:val %) v))
                      pitch-classes)
      ['NaturalPitchClass npc 'Alteration a]
        (pitch-class (kwcat (keyword (:name npc)) (:name a))))
    
    ;;; functions ;;;
    
    (b-meth transpose ['PitchClass 'CInterval] [pc i]
        (let [nat (:name (transpose (:natural pc) (-> i :diatonic :val)))
              v (mod12 (+ (:val pc) (:val i)))]
          (or-dr (pitch-class nat v))))
    
    (b-meth transpose ['PitchClass 'CIntervalClass] [pc ic]
      (transpose pc (c-interval ic)))
    
    (b-fn in-mode? [p m]
      (in? (:pitch-classes m) p))
    
  ;;;;;;;;;;;;;;;;;;;;;; Pitch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def pitches 
      (reduce conj #{}      
        (for [{pcn :name npc :natural pca :alteration} pitch-classes 
               oct (range -6 7)]
          (let [val (+ (* 12 (+ oct 5)) (:pitch-val npc) (:val pca) )]
            (when (between val 0 127 )
              (with-type 
                'Pitch
                {:name (keyword (str (name pcn) oct))
                 :val val
                 :octave oct
                 :pitch-class (pitch-class pcn)}))))))
    
    ; pitches with either no or b alteration
    (def default-name-pitches 
      (let [defaults (set pitch-class-defaults-names)]
        (filter #( defaults (get-in % [:pitch-class :name])) pitches)))
    
    (def name->pitch (reduce #(into %1 {(:name %2) %2}) {} pitches))
    (def val->pitch  (reduce #(into %1 {(:val %2) %2}) {} default-name-pitches))
    
    ;;; Constructor ;;;
    
    (defn- build-pitch [n v o pc]
      (with-type 'Pitch {:name n :val v :octave o :pitch-class pc}))
    
    (b-construct pitch 
                 
      ['Number v] (val->pitch v)
      ['pitch n] (name->pitch n)
      
      ['PitchClass pc] 
        (pitch (kwcat (:name pc) "0"))
      
      ['NaturalPitchClass npc 'Number n]
        (let [[oct mod] (div-mod n 12)
               alt (- mod (:pitch-val npc))
               possible-alt? (between alt -2 2)]
           (cond 
             possible-alt? (pitch (pitch-class npc (alteration alt)) (- oct 5))
             (> alt 0) (pitch (pitch-class npc (alteration (- alt 12))) (- oct 4))
             (< alt 0) (pitch (pitch-class npc (alteration (+ alt 12))) (- oct 6))))  
        
      ['PitchClass p 'Number o]
        (pitch (kwcat (:name p) (str o)))
      
      ['Mode m 'Number n]
        (let [[oct n-mod] (div-mod n 12)
              [[nam dist][nam2 dist2]] 
              (sort-by #(abs (second %)) (map (juxt :name #(- n-mod (:val %1))) (:pitch-classes m)))
              [p1 p2] [(pitch nam (- oct 5)) (pitch nam2 (- oct 5))]]
          ; (debug-repl)
          (if (zero? dist) 
            p1 
            (or (pitch (-> p1 :pitch-class :natural :name) 
                       n) 
                (pitch (-> p2 :pitch-class :natural :name) 
                       n))))
        
      ['NaturalPitchClass npc 'Alteration a 'Number o]
        (pitch (kwcat (:name npc)(:name a) o ))
        
      ; ['Number n 'Mode m]
      ;   (let [n-mod (mod12 n)
      ;         mnpcv (pev (map (c :pitch-val :natural) (:pitch-classes m)))])
    
    )
    
    ;;; functions ;;;
    
    (b-fn distance [p1 p2]
      (abs (- (:val p1) (:val p2))))
    
    (b-fn is-alteration-of [p1 p2]
      (and (= (-> p1 :pitch-class :natural) (-> p2 :pitch-class :natural))
           (= (:octave p1) (:octave p2))))
    
    (b-fn highest [& pitches]
      (best #(> (:val %1)(:val %2)) pitches))
    
    ; (defn alt [p n] 
    ;   (let [a (alteration n)
    ;         int-kw (kwcat (:name a) "1-u")]
    ;     (transpose p (interval int-kw))))
            
    (b-meth transpose ['Pitch 'CInterval] [this ci]
        (let [npc (:natural (transpose (:pitch-class this) ci))
              v (+ (:val this) (:val ci))]
          ; (dr)
          (pitch npc v)))
    
  ;;;;;;;;;;;;;;;;;;;; ModeClass ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def mother-modes
      {:Lyd {
          :degrees [:P1 :M2 :M3 :+4 :P5 :M6 :M7]
          :childs [:Lyd :Mix :Eol :Loc :Ion :Dor :Phry]
          :modes_prio 
            [[:+4 :M7 :M3 :M6 :M2 :P5]
             [:m7 :P4 :M3 :M6 :M2 :P5]
             [:m6 :M2 :P5 :m3 :m7 :P4]
             [:b5 :m2 :m7 :m6 :m3 :P4]
             [:M7 :P4 :M3 :M6 :M2 :P5]
             [:M6 :m3 :m7 :M2 :P5 :P4]
             [:m2 :P5 :P4 :m7 :m3 :m6]]}
       :Lyd+ {
          :degrees [:P1 :M2 :M3 :+4 :+5 :M6 :M7]
          :childs [:Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6]
          :modes_prio 
            [[:+5 :M7 :M3 :+4 :M6 :M2]
             [:+4 :m7 :M3 :M6 :M2 :P5]
             [:m6 :M3 :M2 :P5 :P4 :m7]
             [:b5 :M2 :m3 :m7 :P4 :m6]
             [:b4 :m7 :m6 :m3 :b5 :m2]
             [:M7 :m3 :M6 :M2 :P5 :P4]
             [:M6 :m2 :P4 :m7 :m3 :P5]]}
       :Lyd#2 {
          :degrees [:P1 :#2 :M3 :+4 :P5 :M6 :M7]
          :childs [:Lyd#2 :AltDim :Harmm :Loc6 :Ion+ :Dor+4 :PhryM]
          :modes_prio 
            [[:+4 :#2 :M7 :M3 :M6 :P5]
             [:b4 :o7 :m2 :b5 :m6 :m3]
             [:m6 :M7 :M2 :m3 :P5 :P4]
             [:b5 :M6 :m2 :m7 :m3 :P4]
             [:P4 :+5 :M7 :M3 :M2 :M6]
             [:M6 :+4 :m3 :M2 :m7 :P5]
             [:m2 :M3 :P5 :m7 :m6 :P4]]}})
       
    ;hash-map {mode-name mother-name}
    (def mode-class->mother 
      (apply hash-map 
        (mapcat 
         (fn [[mother-name fields]] 
           (mapcat 
            #(list % mother-name) 
            (:childs fields)))
         mother-modes)))
    
    (def mode-classes
      (set (map
        (fn [[ch-name moth-name]]
          (let [moth-hash (mother-modes moth-name)
                deg-index (->> moth-hash :childs (index-of ch-name))
                deg (c-interval-class (nth (:degrees moth-hash) deg-index))
                prio   (map c-interval-class (-> moth-hash :modes_prio (nth deg-index)))
                degrees (cons (c-interval-class :P1) (vec (sort-by :val prio)))]
            (with-type 
              'ModeClass
              {:name ch-name :mother moth-name :degree deg :prio prio :degrees degrees})))
        mode-class->mother)))
    
    (def name->mode-class (reduce #(into %1 {(:name %2) %2}) {} mode-classes))
    
    ;;; Constructor ;;;
    
    (b-construct mode-class 
      ['mode-class n] (name->mode-class n)
      ['ModeClass m 'Number d] 
        (mode-class (-> mother-modes (get (:name m)) :childs (nth (dec d)))))
    
    ;;; degree moves ;;;
    
    (b-meth nth-diat ['ModeClass 'CIntervalClass 'Number] [mc degree n]
      (let [degs (:degrees mc)]
        (if (in? degs degree)
          (let [d-pos (.indexOf degs degree)]
            (nth degs (mod (+ d-pos n) (count degs))))
            "degree doesn't belongs to mode")))
    
    (b-fn diat-up 
      "args: m ModeClass degree CIntervalClass
      return a CIntervalClass => the diatonic up pitch of `degree`"
      [mc degree]
      (nth-diat mc degree 1))
    
    (b-fn diat-down
      "args: m ModeClass degree CIntervalClass
      return a CIntervalClass => the diatonic down pitch of `degree`"
      [mc degree]
      (nth-diat mc degree -1))
    
    (b-fn degree-step 
      "almost like nth-diat but takes a DInterval as third arg"
      [mc degree di]
      (nth-diat mc degree (:val di)))
    
  ;;;;;;;;;;;;;;;;;;;;;; Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; helpers ;;;
    (declare mode)
    
    (defn- pitch-classes-calc [root degrees]
      (cons root (map #(transpose root (c-interval %)) (next degrees))))
    
    (defn build-mode [n r mc pcs]
      (with-type 'Mode {:name n :root r :mode-class mc :pitch-classes pcs}))
    
    (def mothers 
      (for [mc [:Lyd :Lyd+ :Lyd#2] 
            r [:F :Bb :Eb :Ab :Db :Gb :Cb :Fb
                  :C  :G  :D  :A  :E  :B  :F#]]
        (mode (kwcat r "-" mc))))
    
    ;;; construct ;;;
    
    (b-construct mode
    ['mode n]
      (let [[r mc] (dash-split n)
             r (pitch-class (keyword r))
             mc (mode-class (keyword mc))
             pcs (pitch-classes-calc r (:degrees mc))]
        (build-mode n r mc pcs))
    
    ['Mode m 'Number d]
      (let [mc (mode-class (keyword (second (dash-split (:name m)))) d)
            r (nth (:pitch-classes (mode (:name m))) (dec d))
            n (kwcat (:name r) "-" (:name mc))
            pcs (pitch-classes-calc r (:degrees mc))]
        (build-mode n r mc pcs)))
    
    ;;; methods ;;;
    
    (defn- d-interval-class-val [this]
      (-> this :mode-class :degree :d-class :val))
    
    (defn mother-root [m]
      (let [d (d-interval-class-val m)]
        (first (rotate (:pitch-classes m) (* -1 d)))))
    
    (defn mother-mode [m]
      (mode (kwcat (:name (mother-root m)) "-" (get-in m [:mode-class :mother]))))
    
    (b-meth relative ['Mode 'ModeClass] [m new-mode-class]
      (let [nmc new-mode-class
            degree (-> nmc :degree :d-class :val inc)
            mother-mode-name (kwcat (:name (mother-root m)) "-" (:mother nmc))]
        (mode mother-mode-name degree)))
    
    (b-meth intra-abs-move ['Mode 'Number] [m n]
      (mode (:name (mother-mode m)) n))
    
    (b-meth intra-rel-move ['Mode 'Number] [m n]
      (mode (:name (mother-mode m)) 
            (-> (d-interval-class-val m) (+ n) (mod 7) inc)))
    
    (b-meth transpose ['Mode 'CInterval] [this ci]
      (let [r (transpose (:root this) ci)
            n (kwcat (:name r) "-" (:name (:mode-class this)))
            ps (map #(transpose % ci) (:pitch-classes this))]
        (build-mode n r (:mode-class this) ps)))
    
    ;TODO
    (b-meth nth-diat ['Mode 'PitchClass 'Number] [m pc n]
      (let [pcs (:pitch-classes m)]
        (if (in? pcs pc)
          (let [p-pos (.indexOf pcs pc)]
            (nth pcs (mod (+ p-pos n) (count pcs))))
          "pitch class doesn't belongs to mode")))
    
    (b-meth nth-diat ['Mode 'CIntervalClass 'Number] [m cic n] 
      (nth-diat (:mode-class m) cic n))
    
  ;;;;;;;;;;;;;;;;;; TimeSignature ;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (defn- parse-ts [ts]
      (->> (re-find #"([1-9][1-9]*)\|(2|4|8|16)" ts)
            next
           (map parse-int)))
    
    (defn- build-time-signature
      ([named] (apply build-time-signature (parse-ts (name named)))) 
      ([num den]
        (with-type
          'TimeSignature
          {:name (keyword (str num "|" den))
           :val (* 4 (/ num den))
           :numerator num
           :denominator den})))
    
    (b-construct time-signature
      ['time-signature ts] (build-time-signature ts)
      ['Number n 'Number d] (build-time-signature n d))
    
  ;;;;;;;;;;;;;;; modal-struct-class ;;;;;;;;;;;;;;;;;;;;;;;;

    (def modal-bases
      {:M  [:M3 :P5]    
       :m  [:m3 :P5]    
       :+  [:M3 :+5]    
       :o  [:m3 :b5]    
       :7  [:M3 :P5 :m7]
       :∆  [:M3 :P5 :M7]
       :o7 [:m3 :b5 :o7]
       :ø  [:m3 :b5 :m7]})
    
    (defn- msc-update-degrees [msc]
      (let [msc (dissoc msc :degrees)]
        (assoc msc :degrees (vals msc))))
    
    (defn- msc-assoc [msc & cics]
      (let [msc (ap assoc msc 
                (mapcat #(vector (-> % :d-class :name) %) 
                        cics))]
        (msc-update-degrees msc)))
    
    (defn- msc-dissoc [msc & cics]
      (let [msc (ap dissoc msc 
                 (map 
                  #(if (type= % 'DIntervalClass)
                    (:name %)
                    (-> % :d-class :name)) 
                  cics))]
        (msc-update-degrees msc)))
    
    (defn modal-struct-class 
      ([coll] (a modal-struct-class coll))
      ([x & xs] (ap msc-assoc (with-type 'ModalStructClass {}) x xs)))
    
    (def msc modal-struct-class)
    
    ; (-> (b>> modal-struct-class :m2 :M3 :+4 :m7)
    ;      pev
    ;      (msc-assoc (b> :+5))
    ;      pev
    ;      (msc-assoc (b> :P5))
    ;      pev)

  ;;;;;;;;;;;;;;; modal-struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (declare modal-struct)
    
    (defn ms-assoc [ms & args]
      (let [root (:root ms)
            cics (map #(if (type= % 'PitchClass) 
                         (c-interval-class root %)
                         %) 
                      args)
            msc (ap msc-assoc (:class ms) cics)]
        (modal-struct root msc)))
    
    (defn ms-dissoc [ms & args]
      (let [root (:root ms)
            cics (map #(if (type= % 'PitchClass) 
                         (c-interval-class root %)
                         %) 
                      args)
            msc (ap msc-dissoc (:class ms) cics)]
        (modal-struct root msc)))
   
    (defn modal-struct [root msc]
      (with-type 'ModalStruct
        {:class msc
         :root root
         :pitch-classes 
           (cons root 
            (map (p transpose root) 
                 (:degrees msc)))}))
    
    (def ms modal-struct)

    ; (-> (ms (b> :C) (b>> msc :m2 :M3 :+4 :m7))
    ;      pev
    ;      (ms-assoc (b> :+5))
    ;      pev
    ;      (ms-assoc (b> :G))
    ;      pev)
      
;------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(load-file "src/bartok/parser.clj")
(load-file "src/bartok/user_macros.clj")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;; PRINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use 'clojure.template)
  
  (defn bartok-named-type-print [x]
    (str "<" (symbol (name (:name x))) ">"))

  (do-template [typ]
    (defmethod print-method typ [x ^java.io.Writer w]
      (.write w (bartok-named-type-print x)))
        'Direction
        'Alteration
        'DIntervalClass
        'DInterval
        'CIntervalClass
        'CInterval
        'Pitch
        'PitchClass
        'NaturalPitchClass
        'Mode
        'ModeClass
        'TimeSignature)
  
  ; (defn- nested-types->name [x]
  ;   (letfn [(fun [x]
  ;             (if-let [n (:name x)]
  ;                (symbol (name n))
  ;                (cond
  ;                  (map? x) (map-vals fun x) 
  ;                  (sequential? x) (mapv fun x) 
  ;                  :else x)))]
  ;     (if (map? x) (map-vals fun x) (mapv fun x))))
  
  ; (defmethod print-method 'MelodicDomain [x ^java.io.Writer w]
  ;   (.write w (a str (map #(str (first %) (second %) "\n") (nested-types->name x)))))



  

  
