(ns bartok.litterals.patterns)

(defn- pat-comp [& args]
  (java.util.regex.Pattern/compile (apply str args)))

(def natural-pitch-class-pat 
  #"([A-G])")

(def alteration-pat 
  #"([x#b]b*)")

(def direction-pat 
  #"([ud])")

(def pitch-pat 
  #"([A-G])([x#b]b*)*(\-*[0-5])")

(def pitch-class-pat 
  #"([A-G])([x#b]b*)*")

(def degree-class-pat
  #"(root|second|third|fourth|fifth|sixt|seventh)")

(def degree-pat
  #"([R]|[mM#][23]|[bP+][45]|[mM]6|[omM]7)")

(def generic-interval-class-pat
  #"(1st|2nd|3rd|[4-7]th)")

(def dir-oct-pat
  #"([ud][0-5]*)")

(def dash-pat
  #"(\-)")

(def generic-interval-pat
  (pat-comp generic-interval-class-pat #"(\-)" dir-oct-pat))

(def interval-pat
  ;(pat-comp degree-pat dir-oct-pat)
  #"([mM#][23]|[bP+][45]|[mM]6|[omM]7)(\-)([ud][0-5]*)")

(def mode-class-pat
  #"(Lyd#2|AltDim|Harmm|Loc6|Ion\+|Dor\+4|PhryM|Lyd\+|Lydb7|Mixb6|Loc2|Alt|Melm|Phry6|Lyd|Mix|Eol|Loc|Ion|Dor|Phry)" )

(def mother-mode-pat 
  #"(Lyd#2|Lyd\+|Lyd)")

(def mode-pat
  (pat-comp pitch-class-pat #"(\-)" mode-class-pat))