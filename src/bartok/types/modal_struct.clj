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

(b-meth modal-struct-class 'Mode [m]
  (modal-struct-class (:mode-class m)))

(b-meth modal-struct-class ['Mode clojure.lang.PersistentVector] [m avec]
  (modal-struct-class (:mode-class m) avec))

(b-meth modal-struct-class 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (modal-struct-class (:mode-class m) mains missings))

(b-multi modal-struct)

(b-meth modal-struct 'Mode [m]
  (let [msc (modal-struct-class (:mode-class m))]
    (with-type 'ModalStruct 
      {:class msc
       :mode m
       :main-pitch-classes (map (p transpose (:root m)) (:main-degs msc))
       :aux-pitch-classes (map (p transpose (:root m)) (:aux-degs msc))
       :missing-pitch-classes ()})))

(b-meth modal-struct ['Mode clojure.lang.PersistentVector] [m avec]
  (let [msc (modal-struct-class (:mode-class m) avec)]
    (with-type 'ModalStruct 
      {:class msc
       :mode m
       :main-pitch-classes (map (p transpose (:root m)) (:main-degs msc))
       :aux-pitch-classes (map (p transpose (:root m)) (:aux-degs msc))
       :missing-pitch-classes ()})))

(b-meth modal-struct 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (let [msc (modal-struct-class (:mode-class m) mains missings)]
    (with-type 'ModalStruct 
      {:class msc
       :mode m
       :main-pitch-classes (map (p transpose (:root m)) (:main-degs msc))
       :aux-pitch-classes (map (p transpose (:root m)) (:aux-degs msc))
       :missing-pitch-classes (map (p transpose (:root m)) (:missing-degs msc))})))

