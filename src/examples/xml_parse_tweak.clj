(in-ns 'bartok.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vep (midi-out "Gestionnaire IAC Bus IAC 2" ))

(def sscore (parse-xml "music-files/xml/inv1.xml"))
(def- measure1 (get sscore 1)) 
(def stepss (voice->steps (-> msr1 :voices (get 1))))
(def linee (m-note-line-from (g-pos 0 0 0) 1/2 60 1 (step-sequence (melodic-domain :C-Dor [:C0 :C2] :C1) stepss)))

(def converted-score (convert-notes-and-chords sscore))
(def voice1 (extract-voice converted-score 1))
(def voice5 (extract-voice converted-score 5))
;(play vep (concat (flatten voice1)(flatten voice5)))