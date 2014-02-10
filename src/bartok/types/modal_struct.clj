(ns bartok.types.modal-struct
  (:use utils.all)
  (:use bartok.primitives))

(b-multi modal-struct-class)

(b-meth modal-struct-class 'ModeClass [mc]
  (with-type 'ModalStructClass
    {:mode-class mc
     :main-degs (take 4 (:prio mc))
     :aux-degs (conj (take-last 2 (:prio mc)) (c-interval-class :P1))
     :missing-degs ()}))

(b-meth modal-struct-class ['ModeClass clojure.lang.PersistentVector] [mc avec]
  (let [mains (b> avec)]
    (with-type 'ModalStructClass
      {:mode-class mc
       :main-degs mains
       :aux-degs (conj (remove #(in? mains %) (:prio mc)) (c-interval-class :P1))
       :missing-degs ()})))

(b-meth modal-struct-class 
  ['ModeClass clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [mc mains missings]
  (let [mains (b> mains)
        missings (b> missings)]
    (with-type 'ModalStructClass
      {:mode-class mc
       :main-degs mains
       :aux-degs (conj (remove #(or (in? mains %) (in? missings %)) 
                               (:prio mc)) 
                       (c-interval-class :P1))
       :missing-degs missings})))