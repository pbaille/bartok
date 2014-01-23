(ns bartok.primitives
  (:use utils.all)
  (:use midje.sweet)
  (:require [camel-snake-kebab :as csk])
  (:use vendors.debug-repl))

;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Patterns ;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------
  
  
  (defn- pat-comp [& args]
    (java.util.regex.Pattern/compile (apply str args)))
  
  (def -pat
    #"(\-)")
  
  (def npc-pat 
    #"([A-G])")
  
  (def alt-pat 
    #"(|bb|o|b|m|M|N|P|#|\+|x)")
  
  (def p-alt-pat 
    #"([x#b]b*)*")
  
  (def dir-pat 
    #"([ud])")
  
  (def pitch-pat 
    (pat-comp npc-pat p-alt-pat #"(\-*[0-5])"))
  
  (def pitch-class-pat 
    (pat-comp npc-pat p-alt-pat))
  
  (def cic-pat
    #"([omM#][2367]|[bP+][145])")
  
  (def dic-pat
    #"(1st|2nd|3rd|[4-7]th)")
  
  (def dir-oct-pat
    #"([ud][0-5]*)")
  
  (def d-interval-pat
    (pat-comp dic-pat -pat dir-oct-pat))
  
  (def c-interval-pat
    (pat-comp cic-pat -pat dir-oct-pat))
  
  (def mode-class-pat
    #"(Lyd#2|AltDim|Harmm|Loc6|Ion\+|Dor\+4|PhryM|Lyd\+|Lydb7|Mixb6|Loc2|Alt|Melm|Phry6|Lyd|Mix|Eol|Loc|Ion|Dor|Phry)" )
  
  (def mother-mode-pat 
    #"(Lyd#2|Lyd\+|Lyd)")
  
  (def h-function-pat
    #"(SD|T)(\-|\+|alt)*")
  
  (def mode-pat
    (pat-comp pitch-class-pat #"(\-)" mode-class-pat))
  
  (def time-signature-pat
    #"[1-9][1-9]*\|(2|4|8|16)")
  
;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Identity ;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------
  
  
  (defn- fit? [regex str]
    (if (re-matches regex str) true false))
  
  (defn b? [x]
    ; (pp 'b> x)
    (when (named? x)
      (let [n (name x)
            c (count n)]
        (cond
          (fit? alt-pat n) :alteration
          (fit? dir-pat n) :direction
          (and (symbol? x) (fit? npc-pat n)) :natural-pitch-class
          (fit? cic-pat n) :c-interval-class
          (fit? pitch-class-pat n) :pitch-class
          (fit? pitch-pat n) :pitch
          (fit? c-interval-pat n) :c-interval
          (fit? mode-class-pat n) :mode-class
          (fit? mode-pat n) :mode
          (fit? dic-pat n) :d-interval-class
          (fit? d-interval-pat n) :d-interval
          (fit? h-function-pat n) :h-function
          (fit? time-signature-pat n) :time-signature
          :else nil))))
  
  (defn b-type [x & more]
    (cond
      (named? x) (or (b? x) (type x))
      (number? x) (if (ratio? x) :ratio :number)
      :else (type x)))
  
  (defn b-types 
    ([arg] (b-type arg))
    ([arg & more] (vec (map b-type (concat [arg] more)))))
  
;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------
  
  ; ; types delaration
  ; (declare alteration 
  ;          direction 
  ;          natural_pitch_class 
  ;          pitch_class pitch 
  ;          degree_class 
  ;          degree 
  ;          generic_interval 
  ;          interval 
  ;          mode_class 
  ;          mode 
  ;          time_signature)
  
  (declare comp-b>)
  
  (defn b> 
    ([x] 
     ; (pp 'b> x)
     (when-let [t (b-type x)] 
       (cond 
         (or (= t :number)(= t :ratio)) x
         (keyword? t) (call (name t) x) 
         (fn? x) (comp-b> x)
         (set? x) (set (map b> x))
         (and (not (map? x)) (sequential? x)) (vec (map b> x))
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
  
  (defmacro b-multi [n disp]
    (let [sym (symbol (str "b-multi-" n))]
      `(do 
         (defmulti ~sym ~disp)
         (b-fn ~n [& args#] (a ~sym args#)))))
  
  (defmacro b-method [n disp-val args & body]
    (let [sym (symbol (str "b-multi-" n))]
      `(defmethod ~sym ~disp-val ~args ~@body)))
  
  (defmacro b-multi [n]
    `(do (defmulti ~n b-types)
       (defmethod ~n :default [& args#] 
         (let [b-args# (if (count= args# 1) (b> (first args#)) (b> args#))
               disp-vals# (if (count= args# 1) (b-types b-args#) (a b-types b-args#))]
           (if (contains? (methods ~n) disp-vals#)
             (if (vector? disp-vals#) 
               (a ~n b-args#) 
               (~n b-args#))
             (throw (Exception. 
               (str "*** No dispatch value " 
                    disp-vals# 
                    " for bartok-multimethod " 
                    (name '~n) " ***"))))))))
  
  (defmacro b-construct [n & body]
    `(do (b-multi ~n)
       (defmethod ~n '~(csk/->CamelCase (symbol (name n))) [x#] x#)
       ~@(map (fn [[v & fun-body]]
                 (let [types (vec (take-nth 2 v))
                       args  (vec (take-nth 2 (next v)))
                       types (if (count= types 1) (first types) types)]
                   `(defmethod ~n ~types ~args ~@fun-body))) 
              (partition 2 2 body))))
  
  ;for readibility of b-multi methods
  (defmacro b-meth [& args] `(defmethod ~@args))
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; multi-methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  (b-multi transpose)
  
  ;modal-moves
  (b-multi intra-abs-move)
  (b-multi intra-rel-move)
  (b-multi relative)
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;; generic methods ;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  ;passing
  (defn chrom-up [x] (transpose x :m2-u))
  (defn chrom-down [x] (transpose x :m2-d))
  
;----------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------
  
  ;;;;;;;;;;;;;;;;;;;; Direction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def directions 
       {:up   (with-type 'Direction {:name :u :val 1})
        :down (with-type 'Direction {:name :d :val -1})})
    
    (b-construct direction
      [:direction d]
        (if (= d :u) (directions :up) 
                     (directions :down))
      [:number n]
        (if (= n 1) (directions :up) 
                    (directions :down)))
    
    ;;; Tests ;;;
    
    (fact "direction construct"
      (direction :u) => {:name :u, :val 1}
      (direction 1) => {:name :u, :val 1})

  ;;;;;;;;;;;;;;;;;;;; Alteration ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (defn- make-alterations-set [m]
      (reduce #(conj %1 (with-type 'Alteration (zipmap [:name :val] %2))) #{} m))
    
    (def pitch-alterations    (make-alterations-set {:# 1 :b -1 :x 2 :bb -2 nil 0}))
    (def degree-alterations-1 (make-alterations-set {:o -2 :m -1 :M 0 :# 1}))
    (def degree-alterations-2 (make-alterations-set {:b -1 :P 0 :+ 1}))
    
    (def alterations 
      (merge pitch-alterations 
             degree-alterations-1 
             degree-alterations-2))
    
    (def name->alteration (reduce #(into %1 {(:name %2) %2}) {} alterations))
    (def val->alteration  (reduce #(into %1 {(:val %2) %2}) {} pitch-alterations))
    
    (b-construct alteration
      [:alteration n] (name->alteration n)
      [:number v] (val->alteration v)
      [:number v clojure.lang.Keyword t] 
        (cond (= t :t1) (select-first #(= v (:val %)) degree-alterations-1)
              (= t :t2) (select-first #(= v (:val %)) degree-alterations-2)
              (= t :pitch) (select-first #(= v (:val %)) pitch-alterations)))
    
    ;;; Tests ;;;
    
    (fact "alteration construct"
      (alteration :#) => {:val 1, :name :#}
      (alteration 1) => {:val 1, :name :#}
      (alteration 1 :t2) => {:val 1, :name :+}
      (alteration -2 :t1) => {:val -2, :name :o}
      (alteration 2 :pitch) => {:val 2, :name :x})        
    
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
    
    
    (b-construct d-interval-class
                 
      [:d-interval-class n] 
        (name->d-interval-class n)
        
      ['DInterval gi] (:class gi)
        
      [:number v] 
        (val->d-interval-class v)
        
      ['NaturalPitchClass npc1 'NaturalPitchClass npc2]
        (let [dist (mod (- (:val npc2)(:val npc1)) 7)]
          (d-interval-class dist)))
    
  ;;;;;;;;;;;;;;;;;;;; DInterval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (b-construct d-interval 
                 
      [:d-interval n] 
        (let [[gin diroct] (dash-split n)
               class (d-interval-class (keyword gin))
               [dir oct] (dir-oct-expand diroct)
               val (* (:val dir) (+ (:val class) (* 7 oct)))]
          (with-type 'DInterval 
                     {:name n :val val :class class :direction dir :octave-offset oct}))
        
      [:number v]
        (let [[oct m] (div-mod (abs v) 7)
              class (d-interval-class m)
              dir (direction (if (>= v 0) :u :d))
              n (kwcat (:name class) "-" (:name dir) (if (= 0 oct) "" (str oct)))]
          (with-type 'DInterval 
                     {:name n :val v :class class :direction dir :octave-offset oct}))
        
      ['DIntervalClass n]
        (d-interval (kwcat (:name n) "-u"))
      
      ['DIntervalClass gic 'Direction d]
        (d-interval (kwcat (:name gic) "-" (:name d))))
    
    ;;; Tests ;;;
    
    (fact "d-interval-class construct"
      (d-interval-class :7th) => {:name :7th, :val 6, :degree-val 11, :alt-type :t1}
      (d-interval-class 1) => {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}
      (d-interval-class (d-interval :7th-u)) => {:name :7th, :val 6, :degree-val 11, :alt-type :t1}) 
    
    ;;; Tests ;;;
    
    (fact "d-interval"
      (d-interval :7th-u) 
      => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
      (d-interval 6) 
      => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
      (d-interval (d-interval-class :7th)) 
      => {:name :7th-u, :val 6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :u, :val 1}, :octave-offset 0}
      (d-interval :7th :d) 
      => {:name :7th-d, :val -6, :class {:name :7th, :val 6, :degree-val 11, :alt-type :t1}, :direction {:name :d, :val -1}, :octave-offset 0})
    
  ;;;;;;;;;;;;;;;;;; CIntervalClass ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (def c-interval-classes
        (for [{cn :name cv :val ddv :degree-val ct :alt-type :as dc} d-interval-classes
              alt (cond (= ct :t1) degree-alterations-1
                        (= ct :t2) degree-alterations-2)]
          (let [n (kwcat (:name alt) (-> dc :val inc str))
                v (mod12 (+ ddv (:val alt) 12))] 
            (with-type 'CIntervalClass {:name n :val v :d-class dc}))))
    
    (def d-interval-class->c-interval-class
      (reduce #(into %1 {(-> %2 :d-class :name) %2}) {} 
              (filter #(#{:P1 :M2 :M3 :P4 :P5 :M6 :M7} (:name %)) c-interval-classes)))
    
    (def c-interval-class-default-names 
      #{:P1 :m2 :M2 :m3 :M3 :P4 :+4 :P5 :m6 :M6 :m7 :M7})
    
    (def name->c-interval-class (reduce #(into %1 {(:name %2) %2}) {} c-interval-classes))
    
    (def val->c-interval-class  
      (reduce #(into %1 {(:val %2) %2}) {} 
              (filter #(c-interval-class-default-names (:name %)) c-interval-classes)))
    
    ;;; construct ;;;
    
    (b-construct c-interval-class
      [:c-interval-class n] (name->c-interval-class n)
      [:number v] (val->c-interval-class (mod12 v))
      ['DIntervalClass dc] (d-interval-class->c-interval-class (:name dc))
      ['Mode m] (-> m :mode-class :c-interval-class)
      ['ModeClass m] (:c-interval-class m)
      ['CInterval m] (:class m)
      
      ['DIntervalClass di :number n]
        (let [dv (:degree-val di)
              alt (alteration (- n dv) (:alt-type di))]
          (when alt (c-interval-class (kwcat (:name alt) (inc (:val di))))))
        
      ['PitchClass pc1 'PitchClass pc2]
        (let [dic (d-interval-class (:natural pc1) (:natural pc2))
              diff (mod12 (- (:val pc2)(:val pc1)))]
          ; (dr)
          (c-interval-class dic diff)))
    
    ;;; functions ;;;
    
    (defmethod relative 'CIntervallass [d] 
      (c-interval-class (- 12 (:val d))))
    
    ;;; Tests ;;;
    
    (fact "c-interval-class"
      (c-interval-class :m2) => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
      (c-interval-class 1) => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
      (c-interval-class :2nd) => {:name :M2, :val 2, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
      ;['Mode m] 
      ;['ModeClass  
      ; (c-interval-class :7th 9)     
        
      )
    
  ;;;;;;;;;;;;;;;;;;;; CInterval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (declare pitch)
    
    (defn build-c-interval 
      ([m]
         (let [{:keys [name val direction octave-offset class generic]} m]
           (build-c-interval name val direction octave-offset class generic)))
      ([name val direction octave-offset class gen]
         (with-type 
           'CInterval 
           {:name name 
            :val val 
            :class class 
            :generic gen
            :direction direction 
            :octave-offset octave-offset})))
    
    ;;; construct ;;;
    
    (b-construct c-interval 
      [:c-interval n] 
        (let [[dn diroct] (dash-split n)
               class (c-interval-class (keyword dn))
               [dir oct] (dir-oct-expand diroct)
               gen (-> class :d-class :val (* (:val dir)) (+ (* 7 oct)) d-interval)
               val (* (:val dir) (+ (:val class) (* 12 oct)))]
          (build-c-interval n val dir oct class gen))
      
      ['DInterval gi :number n]
        (let [dc (-> gi :class :name d-interval-class)
              [d-val v] [(:c-interval-class-val dc)(:val dc)]
              alt-n (let [x (- (mod12 (abs n))(mod12 d-val))] 
                      (cond (< x -2) (+ x 12) (> x 2) (- x 12) :else x))
              alt (alteration alt-n (:alt-type dc))
              dir-oct (:octave-offset gi)]
          (c-interval (kwcat (:name alt) (str (inc v)) "-" dir-oct)))
      
      ['DInterval gi]
        (c-interval (kwcat (-> gi :class c-interval-class :name)
                          "-"
                          (-> gi :direction :name)
                          (when-not (zero? (:octave-offset gi)) 
                            (:octave-offset gi)))) 
      
      ['CIntervalClass d] (c-interval (kwcat (:name d) "-u"))
      
      ; ['Pitch p1 'Pitch p2]
      ;   (let [[p1v p2v] (map #(-> % :pitch-class :natural :val) [p1 p2])
      ;          diff (- (:val p2) (:val p1))
      ;          oct-diff (int-div diff 12)
      ;          gicv (if (>= diff 0) (- p2v p1v) (-> (- p2v p1v) (+ 7) (mod 7) - ))
      ;          gen (cond (zero? gicv) 
      ;                      (if (>= diff 0) :1st-u :1st-d)
      ;                    :else (:name (d-interval (+ gicv (* 7 oct-diff)))))]
      ;     (dr)
      ;     (c-interval gen diff))
      ['NaturalPitchClass npc1 'NaturalPitchClass npc2]
        (let [dist (- (:val npc2)(:val npc1))
              gi (d-interval dist)]
          (c-interval gi))
      
      ['PitchClass p1 'PitchClass p2]
        (let [[npc1 npc2] (map :natural [p1 p2])
               npci (c-interval npc1 npc2)])
      
      ['Pitch p1 'Pitch p2]
        (let [[npc1 npc2] (map #(-> % :pitch-class :natural) [p1 p2])
               npci (c-interval npc1 npc2)
               diff (- (:val p2) (:val p1))
               oct-diff (int-div diff 12)
               ]))
    
    ;;; Tests ;;;
    
    (fact "c-interval"
      (c-interval :m2-u1)
      => {:name :m2-u1, :val 13, :class (c-interval-class :m2) :generic (d-interval :2nd-u1), :direction {:name :u, :val 1}, :octave-offset 1}
      (c-interval :2nd-u1)
      => {:name :M2-u1, :val 14, :class (c-interval-class :M2) :generic (d-interval :2nd-u1), :direction {:name :u, :val 1}, :octave-offset 1}
      (c-interval :m2)
      => {:name :m2-u, :val 1, :class (c-interval-class :m2) :generic (d-interval :2nd-u), :direction {:name :u, :val 1}, :octave-offset 0}
      (c-interval-class (c-interval :m2-u1))
      => {:name :m2, :val 1, :d-class {:name :2nd, :val 1, :degree-val 2, :alt-type :t1}}
      ; (c-interval :C :E)
      ;['Pitch p1 'Pitch p2]
    )
  
  ;;;;;;;;;;;;;;;;; NaturalPitchClass ;;;;;;;;;;;;;;;;;;;;;;;
    
    (def natural-pitch-classes
      (map #(with-type 'NaturalPitchClass (zipmap [:pitch-val :val :name] [%3 %2 %1]))
           ['C 'D 'E 'F 'G 'A 'B] (range) [0 2 4 5 7 9 11]))
    
    (def name->natural-pitch-class (reduce #(into %1 {(:name %2) %2}) {} natural-pitch-classes))
    (def val->natural-pitch-class  (reduce #(into %1 {(:val %2) %2}) {} natural-pitch-classes))
    
    ;;; construct ;;;
    
    (b-construct natural-pitch-class 
      [:natural-pitch-class n] (name->natural-pitch-class n)
      [:number v] (val->natural-pitch-class (mod v 7)))
    
    ;;; methods ;;;
    
    (b-meth transpose ['NaturalPitchClass 'DInterval] [this gi]
      (natural-pitch-class (+ (:val this) (:val gi))))
    
    (b-meth transpose ['NaturalPitchClass :number] [this n]
      (natural-pitch-class (+ (:val this) n)))
    
    ;;; Tests ;;;
    
    (fact "natural-pitch-class"
      (natural-pitch-class 'A) => {:name 'A, :val 5, :pitch-val 9}
      (natural-pitch-class 5) => {:name 'A, :val 5, :pitch-val 9}
      (transpose 'A 1) => {:name 'B, :val 6, :pitch-val 11}
      (transpose 'A :2nd-u2) => {:name 'B, :val 6, :pitch-val 11}
      (c-interval 'A 'B)
        => {:name :M2-u, :val 2, 
            :class (c-interval-class :M2) 
            :generic (d-interval :2nd-u) 
            :direction {:name :u, :val 1}, :octave-offset 0}
      (d-interval-class 'A 'B) => {:name :2nd, :val 1, :degree-val 2, :alt-type :t1})
    
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
      [:number v] 
        (val->pitch-class v)
      [:pitch-class n] 
        (name->pitch-class n)
      ['NaturalPitchClass n] 
        (name->pitch-class (keyword (:name n)))
      ['Pitch p] 
        (:pitch-class p)
      ['NaturalPitchClass n :number v] 
        (select-first #(and (= (get-in % [:natural :name]) (:name n))
                            (= (:val %) v))
                      pitch-classes)
      ['NaturalPitchClass npc 'Alteration a]
        (pitch-class (kwcat (keyword (:name npc)) (:name a))))
    
    ;;; functions ;;;
    
    (b-meth transpose ['PitchClass 'CInterval] [pc i]
        (let [nat (:name (transpose (:natural pc) (-> i :generic :val)))
              v (mod12 (+ (:val pc) (:val i)))]
          (pitch-class nat v)))
    
    ;;; Tests ;;;
    
    (fact "pitch-class"
      (pitch-class  2) => {:name :D, :val 2, :natural {:name 'D, :val 1, :pitch-val 2}, :alteration {:val 0, :name nil}} 
      (pitch-class :D) => {:name :D, :val 2, :natural {:name 'D, :val 1, :pitch-val 2}, :alteration {:val 0, :name nil}} 
      (pitch-class 'A) => {:name :A, :val 9, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 0, :name nil}}
      (pitch-class 'A 10) => {:name :A#, :val 10, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 1, :name :#}} 
      (pitch-class 'A :#) => {:name :A#, :val 10, :natural {:name 'A, :val 5, :pitch-val 9}, :alteration {:val 1, :name :#}} 
      ;['Pitch p] 
      (transpose :C# :m2-d) => (pitch-class :B#)
      (c-interval-class  :Bb :C#) => (c-interval-class :#2)   
      (c-interval-class  :C# :Bb) => (c-interval-class :o7) 
      )
    
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
                 
      [:number v] (val->pitch v)
      [:pitch n] (name->pitch n)
      
      ['PitchClass pc] 
        (pitch (kwcat (:name pc) "0"))
      
      ['NaturalPitchClass npc :number n]
        (let [[oct mod] (div-mod n 12)
               alt (- mod (:pitch-val npc))
               possible-alt? (between alt -2 2)]
           ; (debug-repl)
           (when possible-alt? (pitch (pitch-class npc (alteration alt)) (- oct 5))))  
        
      ['PitchClass p :number o]
        (pitch (kwcat (:name p) (str o)))
      
      ['Mode m :number n]
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
        
      ['NaturalPitchClass npc 'Alteration a :number o]
        (pitch (kwcat (:name npc)(:name a) o ))
        
      ; [:number n 'Mode m]
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
          (pitch npc v)))
    
    ;;; Tests ;;;
    
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
      )
    
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
      [:mode-class n] (name->mode-class n)
      ['ModeClass m :number d] 
        (mode-class (-> mother-modes (get (:name m)) :childs (nth (dec d)))))
    
    ;;; Tests ;;;
    
    (fact "mode-class"
      (mode-class :Mix) => (mode-class :Lyd 2))
      
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
    [:mode n]
      (let [[r mc] (dash-split n)
             r (pitch-class (keyword r))
             mc (mode-class (keyword mc))
             pcs (pitch-classes-calc r (:degrees mc))]
        (build-mode n r mc pcs))
    
    ['Mode m :number d]
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
    
    (b-meth intra-abs-move ['Mode :number] [m n]
      (mode (:name (mother-mode m)) n))
    
    (b-meth intra-rel-move ['Mode :number] [m n]
      (mode (:name (mother-mode m)) 
            (-> (d-interval-class-val m) (+ n) (mod 7) inc)))
    
    (b-meth transpose ['Mode 'CInterval] [this ci]
      (let [r (transpose (:root this) ci)
            n (kwcat (:name r) "-" (:name (:mode-class this)))
            ps (map #(transpose % ci) (:pitch-classes this))]
        (build-mode n r (:mode-class this) ps)))
    
    ;;; Tests ;;;
    
    (fact "mode"
      (mode :C-Dor) => (mode :Eb-Lyd 6)
      (mode :A-Loc2) => (transpose :G-Loc2 :M2-u)
      (d-interval-class-val (mode :D-Phry6)) => 6
      (mother-root (mode :B-Loc2)) => (pitch-class :F)
      (mother-mode (mode :B-Loc2)) => (mode :F-Lyd+)
      (relative (mode :D-Dor) :Lyd) => (mode :F-Lyd)
      (intra-abs-move (mode :D-Dor) 3) => (mode :A-Eol)
      (intra-rel-move (mode :D-Dor) 4) => (mode :A-Eol)
      (transpose :B-Dor+4 :M2-u1) => (mode :C#-Dor+4)
      )
    
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
      [:time-signature ts] (build-time-signature ts)
      [:number n :number d] (build-time-signature n d))
    
    ;;; Tests ;;;
    
    (fact "time-signature"
      (time-signature :4|4) => {:name :4|4, :val 4, :numerator 4, :denominator 4}
      (time-signature 4 4) => {:name :4|4, :val 4, :numerator 4, :denominator 4})



