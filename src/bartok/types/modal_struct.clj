(ns bartok.types.modal-struct
  (:use utils.all)
  (:use bartok.primitives))

;-----------------------
;;; WModeClass ;;;
;-----------------------

(b-multi w-mode-class)

(b-meth w-mode-class 'ModeClass [mc]
  (with-type 'WModeClass
    (into mc 
      {:main-degs (take 4 (:prio mc))
       :aux-degs (conj (take-last 2 (:prio mc)) (c-interval-class :P1))
       :missing-degs ()})))

(b-meth w-mode-class ['ModeClass clojure.lang.PersistentVector] [mc avec]
  (let [mains (b> avec)]
    (with-type 'WModeClass
      (into mc 
        {:main-degs mains
         :aux-degs (conj (remove #(in? mains %) (:prio mc)) (c-interval-class :P1))
         :missing-degs ()}))))

(b-meth w-mode-class 
  ['ModeClass clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [mc mains missings]
  (let [mains (b> mains)
        missings (b> missings)]
    (with-type 'WModeClass
      (into mc 
        {:main-degs mains
         :aux-degs (conj (remove #(or (in? mains %) (in? missings %)) 
                                 (:prio mc)) 
                         (c-interval-class :P1))
         :missing-degs missings}))))

(b-meth w-mode-class 'Mode [m]
  (w-mode-class (:mode-class m)))

(b-meth w-mode-class ['Mode clojure.lang.PersistentVector] [m avec]
  (w-mode-class (:mode-class m) avec))

(b-meth w-mode-class 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (w-mode-class (:mode-class m) mains missings))

; ;-----------------------
; ;;;;; WMode ;;;;;;
; ;-----------------------

(b-multi w-mode)

(b-meth w-mode 'Mode [m]
  (let [msc (w-mode-class (:mode-class m))]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes (map (p transpose (:root m)) (:main-degs msc))
         :aux-pitch-classes  (map (p transpose (:root m)) (:aux-degs msc))
         :missing-pitch-classes ()}))))

(b-meth w-mode ['Mode clojure.lang.PersistentVector] [m avec]
  (let [msc (w-mode-class (:mode-class m) avec)]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes (map (p transpose (:root m)) (:main-degs msc))
         :aux-pitch-classes  (map (p transpose (:root m)) (:aux-degs msc))
         :missing-pitch-classes ()}))))

(b-meth w-mode 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (let [msc (w-mode-class (:mode-class m) mains missings)]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes    (map (p transpose (:root m)) (:main-degs msc))
         :aux-pitch-classes     (map (p transpose (:root m)) (:aux-degs msc))
         :missing-pitch-classes (map (p transpose (:root m)) (:missing-degs msc))}))))

