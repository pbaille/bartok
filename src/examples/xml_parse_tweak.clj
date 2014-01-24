(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(def sscore (bartok.midi.xml-parser/main "music-files/xml/inv1.xml"))
(def- msr1 (get sscore 1)) 
(def stepss (voice->steps (-> msr1 :voices (get 1))))
(def linee (ap m-note-line-from (g-pos 0 0 0) 1/2 60 1 (step-sequence (melodic-domain :C-Dor [:C0 :C2] :C1) stepss)))
