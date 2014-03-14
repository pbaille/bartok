(in-ns 'bartok.primitives)

;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Patterns ;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------

  (def -pat
    #"(\-)")
  
  (def npc-pat 
    #"([A-G])")
  
  (def alt-pat 
    #"(bb|o|b|m|M|N|P|#|\+|x|)")
  
  (def p-alt-pat 
    #"(#|b|bb|x)?")
  
  (def dir-pat 
    #"([ud])")
  
  (def pitch-pat 
    (re-cat npc-pat p-alt-pat #"(\-?[0-5])"))
  
  (def pitch-class-pat 
    (re-cat npc-pat p-alt-pat))
  
  (def cic-pat
    #"([omM#][2367]|[bP+][145])")
  
  (def dic-pat
    #"(1st|2nd|3rd|[4-7]th)")
  
  (def dir-oct-pat
    #"([ud][0-5]*)")
  
  (def d-interval-pat
    (re-cat dic-pat -pat dir-oct-pat))
  
  (def c-interval-pat
    (re-cat cic-pat -pat dir-oct-pat))

  (def mode-class-pat
    (re-pattern 
     (str
      "(Lyd#2|AltDim|Harmm|Loc6|Ion\\+|Dor\\+4|PhryM|"
      "Lyd\\+|Lydb7|Mixb6|Loc2|Alt|Melm|Phry6|"
      "Lyd|Mix|Eol|Loc|Ion|Dor|Phry)")))
  
  (def mother-mode-pat 
    #"(Lyd#2|Lyd\+|Lyd)")
  
  (def h-function-pat
    #"(SD|T)(\-|\+|alt)?")
  
  (def mode-pat
    (re-cat pitch-class-pat -pat mode-class-pat))
  
  (def time-signature-pat
    #"[1-9][1-9]*\|(2|4|8|16)")
  
;-------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;; Identity ;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------
  
  (defn- fit? [regex str]
    (when (re-matches regex str) true))
  
  (defn b? [x]
    (when (named? x)
      (let [n (name x)]
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
  