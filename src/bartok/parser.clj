(in-ns 'bartok.primitives)

;;;;;;; before insta ;;;;;;;;;;;;

  ; (def ^:private escaped-chars ["+" "." "?" "|" "$" "^"])

  ; (defn escape-regex-reserved [str] 
  ;   (-> str
  ;     (clojure.string/replace #"\+" "\\\\+")
  ;     (clojure.string/replace #"\." "\\\\.")
  ;     (clojure.string/replace #"\?" "\\\\?")
  ;     (clojure.string/replace #"\|" "\\\\|")
  ;     (clojure.string/replace #"\$" "\\\\$")
  ;     (clojure.string/replace #"\^" "\\\\^")))

  ; (defn re-wrap 
  ;   "wrap a regex ex: (re-wrap #\"a|b|c\") => #\"(a|b|c)\""
  ;   [re] 
  ;   (re-pattern (str "(" (str re) ")")))

  ; (defn re-unwrap 
  ;   "unwrap a regex if it's wrapped 
  ;   ex: (re-wrap #\"(a|b|c)\") => #\"a|b|c\""
  ;   [re] 
  ;   (let [s (str re)] 
  ;     (if (and (= \( (first s))(= \) (last s))) 
  ;       (re-pattern (a str (next (butlast s))))
  ;       re)))

  ; (defn re-wrapv 
  ;   "wrap a regex ex: (re-wrap #\"a|b|c\") => #\"[a|b|c]\""
  ;   [re] 
  ;   (re-pattern (str "[" (str re) "]")))

  ; (defn re-unwrapv 
  ;   "unwrap a regex if it's wrapped 
  ;   ex: (re-wrap #\"[a|b|c]\") => #\"a|b|c\""
  ;   [re] 
  ;   (let [s (str re)] 
  ;     (if (and (= \[ (first s))(= \] (last s))) 
  ;       (re-pattern (a str (next (butlast s))))
  ;       re)))

  ; (a str (next (butlast "(123)")))

  ; (defn re-? 
  ;   "add ? at the end of a regex"
  ;   [re] (re-pattern (str (str re) "?")))

  ; (defn re-or 
  ;   "ex: (re-or #'(a|b)' #'(c|d)') => #'(a|b)|(c|d)' "
  ;   [& pats]
  ;   (re-pattern (a str (interpose "|" (map str pats)))))

  ; (defn coll->or-pat 
  ;   "ex : (coll->or-pat [:a :b]) => #'a|b' "
  ;   [coll]
  ;   (->> coll
  ;     (map (c escape-regex-reserved name))
  ;     (sort-by count)
  ;     reverse
  ;     (interpose "|")
  ;     (a str)
  ;     re-pattern))

  ; (defn syn-resolver [m]
  ;   (let [res (reduce-kv
  ;              #(a assoc %1 
  ;                  (mapcat (fn [x] (vector x %2)) %3)) 
  ;              (zipmap (keys m) (keys m))
  ;              m)]
  ;     (fn [x] (res x))))

  ; ;((syn-resolver p-alt-syns)  :sharp)

  ; (defn syn-pat [m]
  ;   (coll->or-pat (concat (keys m)(a concat (vals m)))))

  ; (declare litteral-type)
  ; (defn- lit? 
  ;   "return an optional version of given litteral-type"
  ;   [lit]
  ;   (let [lit (litteral-type lit)]
  ;     (update-in lit [:pattern] re-?)))

  ; (defn- lit-or 
  ;   "return an optional version of given litteral-type"
  ;   [& lits]
  ;   (let [lits (map litteral-type lits)]
  ;     (with-type :litteral-type
  ;       {:pattern (re-wrap 
  ;                  (a re-or
  ;                   (map (c re-unwrap :pattern) lits)))
  ;        :resolver #(first (keep (fn [fun] (fun %)) 
  ;                                (map :resolver lits)))})))

  ; ; (match? (lit-or t1-alt-syns t2-alt-syns) :m)

  ; (defn- c-litteral-type 
  ;   [& lit-types]
  ;   ; (dr)
  ;   (let [resolvers (mapcat #(or (:resolvers %) [(:resolver %)]) lit-types)]
  ;     (with-type :litteral-type
  ;       {:pattern (a re-cat (map :pattern lit-types)) 
  ;        :resolver (fn [& xs] (map #(%1 %2) resolvers xs))
  ;        :resolvers resolvers})))

  ; (defn litteral-type 
  ;   ([x]
  ;    (cond 
  ;      (type= x :litteral-type) x
  ;      (vector? x) 
  ;        (with-type :litteral-type
  ;          {:pattern (re-wrap (coll->or-pat x)) 
  ;           :resolver #((set x) %)})
  ;      (map? x) 
  ;        (with-type :litteral-type
  ;          {:pattern (re-wrap (syn-pat x)) 
  ;           :resolver (syn-resolver x)})))
  ;   ([x & xs]
  ;    ; (dr)
  ;    (a c-litteral-type (map litteral-type (cons x xs)))))

  ; (defn match? [lit-type x]
  ;   (if-let [m (re-matches (:pattern lit-type) (name x))]
  ;     (a (:resolver lit-type) (map keyword (next m)))))

  ; (match? (litteral-type p-alt-syns) :#)

  ; (def alt-lit (litteral-type p-alt-syns))
  ; (def cic-lit (litteral-type alt-lit dic-syns))

  ; (match? cic-lit :#3)

  ; (def cic (litteral-type  
  ;            (lit? (lit-or t1-alt-syns t2-alt-syns)) 
  ;            dic-syns))

  ; (match? (litteral-type cic alt-lit) :minorthird#)

  ; (match? (litteral-type [:C :D :E :F :G :A :B] 
  ;                        (lit? (lit-or t1-alt-syns t2-alt-syns))) 
  ;         :C)

(require '[instaparse.core :as insta])
 
;;;;;;;;; data ;;;;;;;;;;;;;

  (def dir-syns
    {:u [:up]
     :d [:down :dwn]})

  (def p-alt-syns
    {:# [:sharp]
     :b [:flat]
     :bb [:double-flat :dbl-flat]
     :x [:double-sharp :dbl-sharp :##]})

  (def t1-alt-syns
    {:P [:perfect]
     :+ [:augmented :aug]
     :b [:flat]})

  (def t2-alt-syns
    {:M [:maj :major]
     :# [:sharp]
     :m [:- :min :minor]
     :o [:dim :diminished]})

  (def dic-syns
    {:1st [:tonic :root :1 :8]
     :2nd [:second :2 :9]
     :3rd [:third :3 :10]
     :4th [:fourth :4 :11]
     :5th [:fifth :5 :12]
     :6th [:sixth :6 :13]
     :7th [:seventh :7 :14]})
  
  (def mc-syns 
    {:Lyd [:lydian]
     :Mix [:mixolydian]
     :Eol [:aeolian :eolian]
     :Loc [:locrian] 
     :Ion [:ionian]
     :Dor  [:dorian]
     :Phry [:phrygian]
     :Melm [:melodic-minor]
     :Alt  [:altered :super-locrian]
     :Harmm [:harmonic-minor]})
  
  (def modal-base-syns
    {:M [:Major :major :Maj :maj]
     :m [:Minor :minor :Min :min :-]
     :+ [:Aug :aug]
     :o [:Dim :dim]
     :7 [:Dominant :dominant :Seventh :seventh :Dom :dom :7th]
     :∆ [:M7 :maj7 :Maj7 :major7 :Major7 :∆7]
     :o7[:dim7 :Dim7]
     :ø [:Ø]})
  
;;;;;;;;; utils ;;;;;;;;;;;;

  (defn escape-regex-reserved [str] 
    (-> str
      (clojure.string/replace #"\+" "\\\\+")
      (clojure.string/replace #"\." "\\\\.")
      (clojure.string/replace #"\?" "\\\\?")
      (clojure.string/replace #"\|" "\\\\|")
      (clojure.string/replace #"\$" "\\\\$")
      (clojure.string/replace #"\^" "\\\\^")))

  (defn coll->or-str
    "ex : (coll->or-pat [:a :b]) => #'a|b' "
    [coll]
    (->> coll
      (map (c escape-regex-reserved name))
      (sort-by count)
      reverse
      (interpose "|")
      (a str)))

  (defn syn-map->or-str [m]
    (coll->or-str (concat (keys m)(a concat (vals m)))))

  (defn syn-resolver [m]
    (let [res (reduce-kv
               #(a assoc %1 
                   (mapcat (fn [x] (vector x %2)) %3)) 
               (zipmap (keys m) (keys m))
               m)]
      (fn [x] (res x))))

  (defn instapat 
    "turn a string into a instaparse compatible regex"
    [s]
    (str "#'" s "'"))  
  
  (defn re->instapat
    "turn a regex into instaparse regex"
    [re]
    (str "#'" (str re) "'"))

  (defn insta-primitive
    [nam x]
    (cond 
      (vector? x) 
        {:insta-str (str (name nam) " = " (instapat (coll->or-str x)) " ") 
         :resolver #((set x) %)
         :name (keyword nam)}
      (map? x) 
        {:insta-str (str (name nam) " = " (instapat (syn-map->or-str x)) " ") 
         :resolver (syn-resolver x)
         :name (keyword nam)}))

;;;;;;;;;;; evaluators ;;;;;;;;;

  (defn natural-pitch-class* [str] (natural-pitch-class (symbol str)))
  (defn time-signature* [& args] (a time-signature (map parse-int args)))
  (defn direction* [s] (direction (keyword s)))
  (defn alteration* [s] (alteration (keyword s)))
  (defn d-interval-class* [s] 
    (d-interval-class (keyword s)))
  (defn d-interval* 
    ([dic] (d-interval dic))
    ([dic dir] (d-interval dic dir))
    ([dic dir oct] 
     (d-interval 
      (kwcat (:name dic) "-" (:name dir) (parse-int oct)))))
  (defn c-interval-class* 
    ([dic] (if (string? dic) 
             (c-interval-class :M7) ;for handling ∆ and ∆7 cases... have to find better
             (c-interval-class dic)))
    ([alt dic] (c-interval-class dic alt)))
  (defn c-interval* 
    ([cic] (c-interval cic))
    ([cic dir] (c-interval cic dir))
    ([cic dir oct] 
     (c-interval 
      (kwcat (:name cic) "-" (:name dir) (parse-int oct)))))
  (defn pitch-class* 
    ([npc] (pitch-class npc))
    ([npc alt] (pitch-class npc alt)))
  (defn pitch* 
    ([pc] (pitch-class pc))
    ([pc oct] (pitch pc (parse-int oct))))
  (defn mother-mode-class* []); TODO
  (defn mode* []); TODO
  
  (defn modal-struct-class* 
    ([[k & values]] 
     (let [base (when (= k :modal-base)
                  (b> (modal-bases (keyword (first values)))))
           mods (when-not base values)] 
       (if base 
         (a modal-struct-class base) 
         (modal-struct-class* [:base :M] (cons :modal-modifiers mods)))))
    ([[_ base] [_ & mods]]
     (let [msc (modal-struct-class 
                 (b> (modal-bases (keyword base))))
           omits (->> (filter vector? mods) 
                      (map second) 
                      (map #(if (= % "sus") (b> :3rd) %)))
           cics (filter #(type= % 'CIntervalClass) mods)]
       (as-> msc x 
        (ap msc-assoc x cics)
        (ap msc-dissoc x omits)))))
  
  (defn modal-struct* 
    ([root] (modal-struct root (b>> modal-struct-class :M3 :P5)))
    ([root msc] (modal-struct root msc)))
  
  ; (pp "\n" (b-parser "Cm∆+4"))
  
;;;;;;;;; eval-tree ;;;;;;;;;;;;
  
  (defn eval-tree [tree]
    (insta/transform
      {:natural-pitch-class natural-pitch-class*
       :time-signature time-signature*
       :p-alt alteration*
       :alt-t1 alteration*
       :alt-t2 alteration*
       :direction direction*
       :d-interval-class d-interval-class*
       :d-interval d-interval*
       :c-interval-class c-interval-class*       
       'c-interval c-interval*
       :pitch-class pitch-class*
       :pitch pitch*
       ; :mode-class mode-class*
       ; :mode mode*
   
       :modal-struct-class modal-struct-class*
       :modal-struct modal-struct*
       }
      tree))
  
;;;;;;;;;; main ;;;;;;;;;;;;

  (def main-gram
    "<S> = time-signature 
         | direction 
         | alteration 
         | c-interval-class 
         | c-interval 
         | d-interval-class 
         | d-interval 
         | natural-pitch-class
         | pitch-class 
         | pitch 
         | mode-class 
         | mode
         | modal-struct-class
         | modal-struct
      
     (* *)
    
       (* time-signature helpers *)
       <ts-num> = #'\\d{1,2}'
       <ts-den> = #'2|4|8|16'
      
       <dir-oct> = <'-'>? direction #'0|1|2|3|4|5'?
       <alteration> = alt-t1 | alt-t2 | p-alt
      
       (* modal-struct helpers *)
       omission = (<'omit'> c-interval-class) | 'sus'
       modal-modifiers = (c-interval-class | omission )+
       
       (* types *)
       natural-pitch-class = #'A|B|C|D|E|F|G'
       time-signature = ts-num <('/'|'|')> ts-den
       d-interval = d-interval-class dir-oct
       c-interval-class = #'∆7|∆' | ((alt-t1 | alt-t2)?  d-interval-class )
       c-interval = c-interval-class dir-oct 
       pitch-class = natural-pitch-class p-alt? 
       pitch = pitch-class #'\\-?[0-5]'
       mode-class = mother-mode-class modal-modifiers?
       mode = pitch-class mode-class
       modal-struct-class = (modal-base modal-modifiers?) / modal-modifiers
       modal-struct = pitch-class modal-struct-class?
    ")

  (defn bartok-parser [main-gram primitives]
    (let [prims (map (fn [[k v]] (insta-primitive k v)) primitives)
          transforms 
          (reduce #(assoc %1 
                    (:name %2) 
                    (fn [x] 
                      [(:name %2) (name ((:resolver %2) (keyword x)))])) 
                  {}
                  prims)
          parser (insta/parser 
                  (ap str main-gram 
                   (map :insta-str prims)))] 
      (fn [x] 
        ; (dr)
        (->> (insta/parses parser x)
          (map first)
          (insta/transform transforms)
          eval-tree))))

  (def b-parser
    (bartok-parser 
     main-gram
     {:alt-t1 t1-alt-syns
      :alt-t2 t2-alt-syns
      :p-alt p-alt-syns
      :direction dir-syns
      :d-interval-class dic-syns 
      :mother-mode-class mc-syns
      :modal-base modal-base-syns }))
  
  (defn parses-map [x] 
    (reduce #(assoc %1 (first %2) (second %2)) 
            {} 
            (map-h #(hash-map %1 (first %2)) 
                   (group-by b-type (b-parser (name x))))))
  
;;;;;;;;;;; ex ;;;;;;;;;;;;;

  ;(pp "\n" (b-parser "m∆9omit5"))
  ; (pp "\n" (b-parser "D#"))
  

  
  ; (defn tree->expr [[head & nodes]]
  ;   (if (and (count= nodes 1) (not (vector? (first nodes))))
  ;     (call (name head) (first nodes))
  ;     (a call (name head) (map tree->expr nodes))))  
 
 ; (re-matches #"(M|m|o|\+)(?:\-?)((?:bb|o|b|m|M|N|P|#|\+|x)?(?:[2-9]|10|11|13))+" "Mm2P52")

  