(ns bartok.litterals.alt
  (:use utils.all)
  (:use vendors.debug-repl)
  (:use [bartok.multimethods]))

;************ Patterns ***************

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

(def deg-class-pat
  #"(root|second|third|fourth|fifth|sixt|seventh)")

(def deg-pat
  #"([omM#][2367]|[bP+][145])")

(def gic-pat
  #"(1st|2nd|3rd|[4-7]th)")

(def dir-oct-pat
  #"([ud][0-5]*)")

(def generic-interval-pat
  (pat-comp gic-pat -pat dir-oct-pat))

(def interval-pat
  (pat-comp deg-pat -pat dir-oct-pat))

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

;*********************** Identity *********************************

(defn- fit? [regex str]
  (if (re-matches regex str) true false))

(defn b? [x]
  (when (named? x)
    (let [n (name x)
          c (count n)]
      (cond
        (fit? alt-pat n) :alteration
        (fit? dir-pat n) :direction
        (and (symbol? x) (fit? npc-pat n)) :natural-pitch-class
        (fit? deg-pat n) :degree
        (fit? pitch-class-pat n) :pitch-class
        (fit? pitch-pat n) :pitch
        (fit? interval-pat n) :interval
        (fit? mode-class-pat n) :mode-class
        (fit? mode-pat n) :mode
        (fit? gic-pat n) :generic-interval-class
        (fit? generic-interval-pat n) :generic-interval
        (fit? deg-class-pat n) :degree-class
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

;********************** Eval ***************************

; types delaration
(declare alteration 
         direction 
         natural_pitch_class 
         pitch_class pitch 
         degree_class 
         degree 
         generic_interval 
         interval 
         mode_class 
         mode 
         time_signature)

(declare comp-b>)

(defn b> 
  ([x] (when-let [t (b-type x)] 
         (cond 
           (or (= t :number)(= t :ratio)) x
           (keyword? t) (call (name t) x) 
           (fn? x) (comp-b> x)
           (set? x) (set (map b> x))
           (and (not (map? x)) (seq x)) (vec (map b> x))
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

(defmacro b-construct [n & body]
  `(do (defmulti ~n b-types)
     ~@(map (fn [[v & fun-body]]
               (let [types (vec (take-nth 2 v))
                     args  (vec (take-nth 2 (next v)))
                     types (if (count= types 1) (first types) types)]
                 `(defmethod ~n ~types ~args ~@fun-body))) 
            (partition 2 2 body))
     (defmethod ~n '~(camel-snake-kebab/->CamelCase (symbol (name n))) [x#] x#)
     (defmethod ~n :default [& args#] 
        ; (debug-repl)
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

;************** Types ********************************

(load "types_alt/alteration"
      "types_alt/direction"
      "types_alt/natural_pitch_class"
      "types_alt/pitch_class"
      "types_alt/pitch"
      "types_alt/degree_class"
      "types_alt/degree"
      "types_alt/generic_interval"
      "types_alt/interval"
      "types_alt/mode_class"
      "types_alt/mode"
      "types_alt/time_signature")

