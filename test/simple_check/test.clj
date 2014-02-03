(ns simple-check.test
  (:use utils.all)
  (:require [simple-check.core :as sc])
  (:require [simple-check.generators :as gen])
  (:require [simple-check.properties :as prop]))

; (def sort-idempotent-prop
;   (prop/for-all [v (gen/vector gen/int)]
;     (= (sort v) (sort (sort v)))))

; (sc/quick-check 100 sort-idempotent-prop)
; ;; => {:result true, :num-tests 100, :seed 1382488326530}

; (gen/sample gen/any)
; (partition 3 3 (gen/sample (gen/frequency [[5 gen/int] [3 (gen/vector gen/int)] [2 gen/boolean]]) 100))

; (map (p kwcat "yo") (gen/sample (gen/elements [:C :D :E])))

; (gen/sample 
;   (gen/fmap vec gen/int gen/boolean))

; ; GUIDE HERE!!!
; https://github.com/reiddraper/simple-check/blob/master/doc/intro.md

