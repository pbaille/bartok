(ns bartok.litterals.patterns)

(def natural-pitch-class-name 
  #"([A-G])")

(def alteration-name 
  #"([x#b]b*)")

(def pitch-name 
  #"([A-G])([x#b]b*)*(\-*[0-5])")

(def pitch-class-name 
  #"([A-G])([x#b]b*)*")

(def generic-interval-class-name
  #"(unison|second|third|fourth|fifth|sixt|seventh)")

(def generic-interval-name
  #"(unison|second|third|fourth|fifth|sixt|seventh)([0-9])")

(def interval-class-name
  #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)")

(def interval-name
  #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)([ud][0-5]*)")

(def mode-class-name
  #"(Lyd#2|AltDim|Harmm|Loc6|Ion\+|Dor\+4|PhryM|Lyd\+|Lydb7|Mixb6|Loc2|Alt|Melm|Phry6|Lyd|Mix|Eol|Loc|Ion|Dor|Phry)" )

(def mother-mode-name 
  #"(Lyd#2|Lyd\+|Lyd)")

(def mode-name
  (java.util.regex.Pattern/compile
    (str pitch-class-name #"(\-)" mode-class-name)))