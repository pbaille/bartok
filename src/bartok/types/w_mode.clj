(ns bartok.types.w-mode
  (:use utils.all)
  (:use bartok.primitives))

;-----------------------
;;; WModeClass ;;;
;-----------------------
(b-multi w-mode-class)

(b-meth w-mode-class 'ModeClass [mc]
  (with-type 'WModeClass
    (into mc 
      {:main-degrees (take 4 (:prio mc))
       :aux-degrees (conj (take-last 2 (:prio mc)) (c-interval-class :P1))
       :missing-degrees ()})))

(b-meth w-mode-class ['ModeClass clojure.lang.PersistentVector] [mc avec]
  (let [mains (b> avec)]
    (with-type 'WModeClass
      (into mc 
        {:main-degrees mains
         :aux-degrees (conj (remove #(in? mains %) (:prio mc)) (c-interval-class :P1))
         :missing-degrees ()}))))

(b-meth w-mode-class 
  ['ModeClass clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [mc mains missings]
  (let [mains (b> mains)
        missings (b> missings)]
    (with-type 'WModeClass
      (into mc 
        {:main-degrees mains
         :aux-degrees (conj (remove #(or (in? mains %) (in? missings %)) 
                                 (:prio mc)) 
                         (c-interval-class :P1))
         :missing-degrees missings}))))

(b-meth w-mode-class 'Mode [m]
  (w-mode-class (:mode-class m)))

(b-meth w-mode-class ['Mode clojure.lang.PersistentVector] [m avec]
  (w-mode-class (:mode-class m) avec))

(b-meth w-mode-class 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (w-mode-class (:mode-class m) mains missings))

;-----------------------
;;;;; WMode ;;;;;;
;-----------------------

(b-multi w-mode)

(b-meth w-mode 'Mode [m]
  (let [msc (w-mode-class (:mode-class m))]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes (map (p transpose (:root m)) (:main-degrees msc))
         :aux-pitch-classes  (map (p transpose (:root m)) (:aux-degrees msc))
         :missing-pitch-classes ()}))))

(b-meth w-mode ['Mode clojure.lang.PersistentVector] [m avec]
  (let [msc (w-mode-class (:mode-class m) avec)]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes (map (p transpose (:root m)) (:main-degrees msc))
         :aux-pitch-classes  (map (p transpose (:root m)) (:aux-degrees msc))
         :missing-pitch-classes ()}))))

(b-meth w-mode 
  ['Mode clojure.lang.PersistentVector clojure.lang.PersistentVector] 
  [m mains missings]
  (let [msc (w-mode-class (:mode-class m) mains missings)]
    (with-type 'WMode 
      (into (dissoc m :mode-class) 
        {:w-mode-class msc
         :main-pitch-classes    (map (p transpose (:root m)) (:main-degrees msc))
         :aux-pitch-classes     (map (p transpose (:root m)) (:aux-degrees msc))
         :missing-pitch-classes (map (p transpose (:root m)) (:missing-degrees msc))}))))

;----------------
;;;; methods ;;;;
;----------------

  ;smelly hack on types... have to make a hierarchy but don't know how with those symbol types...
  (b-meth nth-diat ['WModeClass 'CIntervalClass :number] [wmc degree n]
    (nth-diat (with-type 'ModeClass wmc) degree n))
  
  (b-meth nth-diat ['WMode 'CIntervalClass :number] [wm degree n]
    (nth-diat (with-type 'ModeClass (:w-mode-class wm)) degree n))
  
  (b-meth nth-diat ['WMode 'PitchClass :number] [wm pc n]
    (nth-diat (with-type 'Mode wm) pc n))
  
  ;;;
  (b-multi main-degree?
    "return true if deg is a main-degree of w-mode-class wmc")
  
  (b-meth main-degree? ['WModeClass 'CIntervalClass] [wmc deg] 
    (in? (:main-degrees wmc) deg))
  
  (b-meth main-degree? ['WMode 'CIntervalClass] [wm deg] 
    (in? (:main-degrees (:w-mode-class wm)) deg))
  
  (b-fn main-pitch-class? 
    "return true if pc is a main-pitch-class of w-mode wm"    
    [wm pc] 
    (in? (:main-pitch-classes wm) pc))
  
  ;;;
  (b-multi aux-degree?
    "return true if deg is a aux-degree of w-mode-class wmc")
  
  (b-meth aux-degree? ['WModeClass 'CIntervalClass] [wmc deg] 
    (in? (:aux-degrees wmc) deg))
  
  (b-meth aux-degree? ['WMode 'CIntervalClass] [wm deg] 
    (in? (:aux-degrees (:w-mode-class wm)) deg))
  
  (b-fn aux-pitch-class? 
    "return true if pc is a aux-pitch-class of w-mode wm"    
    [wm pc] 
    (in? (:aux-pitch-classes wm) pc))
  
  ;;;
  (b-multi missing-degree?
    "return true if deg is a missing-degree of w-mode-class wmc")
  
  (b-meth missing-degree? ['WModeClass 'CIntervalClass] [wmc deg] 
    (in? (:missing-degrees wmc) deg))
  
  (b-meth missing-degree? ['WMode 'CIntervalClass] [wm deg] 
    (in? (:missing-degrees (:w-mode-class wm)) deg))
  
  (b-fn missing-pitch-class? 
    "return true if pc is a missing-pitch-class of w-mode wm"
    [wm pc] 
    (in? (:missing-pitch-classes wm) pc))

  ;;;
  (b-multi chromatic-up
    "return the first adjacent up chromatic-degree/pitch-class or nil")
  
  (b-meth chromatic-up ['WModeClass 'CIntervalClass] [wmc deg] 
    (when-not (in? (:degrees wmc) (b:+ deg :m2)) (b:+ deg :+1)))
  
  (b-meth chromatic-up ['WMode 'CIntervalClass] [wm deg] 
    (when-not (in? (:degrees (:w-mode-class wm)) (b:+ deg :m2)) (b:+ deg :+1)))
  
  (b-meth chromatic-up ['WMode 'PitchClass] [wm pc] 
    (when-not (in? (:pitch-classes wm) (transpose pc :m2)) (transpose pc :+1)))
  
  ;;;
  (b-multi chromatic-down
    "return the first adjacent down chromatic-degree/pitch-class or nil")
  
  (b-meth chromatic-down ['WModeClass 'CIntervalClass] [wmc deg] 
    (when-not (in? (:degrees wmc) (b:- deg :m2)) (b:- deg :+1)))
  
  (b-meth chromatic-down ['WMode 'CIntervalClass] [wm deg] 
    (when-not (in? (:degrees (:w-mode-class wm)) (b:- deg :m2)) (b:- deg :+1)))
  
  (b-meth chromatic-down ['WMode 'PitchClass] [wm pc] 
    (when-not (in? (:pitch-classes wm) (transpose pc :m2-d)) (transpose pc :b1)))
  
  ;;;
  (b-multi main-up
    "return the first adjacent up main-degree/pitch-class")
  
  (b-meth main-up ['WModeClass 'CIntervalClass] [wmc deg] 
    (or (select-first #(> (:val %) (:val deg)) (->> wmc :main-degrees (sort-by :val)))
        (->> wmc :main-degrees (sort-by :val) first)))
  
  (b-meth main-up ['WMode 'CIntervalClass] [wm deg] 
    (or (select-first #(> (:val %) (:val deg)) (->> wm :w-mode-class :main-degrees (sort-by :val)))
        (->> wm :w-mode-class :main-degrees (sort-by :val) first)))
  
  (b-meth main-up ['WMode 'PitchClass] [wm pc] 
    (or (select-first #(> (:val %) (:val pc)) (->> wm :main-pitch-classes (sort-by :val)))
        (->> wm :main-pitch-classes (sort-by :val) first)))
  
  ;;;
  (b-multi main-down
    "return the first adjacent down main-degree/pitch-class")
  
  (b-meth main-down ['WModeClass 'CIntervalClass] [wmc deg] 
    (or (last (filter #(< (:val %) (:val deg)) 
                      (->> wmc :main-degrees (sort-by :val))))
        (->> wmc :main-degrees (sort-by :val) last)))
  
  (b-meth main-down ['WMode 'CIntervalClass] [wm deg] 
    (or (last (filter #(< (:val %) (:val deg)) 
                      (->> wm :w-mode-class :main-degrees (sort-by :val))))
        (->> wm :w-mode-class :main-degrees (sort-by :val) last)))
  
  (b-meth main-down ['WMode 'PitchClass] [wm pc] 
    (or (last (filter #(< (:val %) (:val pc)) 
                      (->> wm :main-pitch-classes (sort-by :val))))
        (->> wm :main-pitch-classes (sort-by :val) last)))
  
  ;;;
  (b-multi aux-up
    "return the first adjacent up aux-degree/pitch-class or nil")
  
  (b-meth aux-up ['WModeClass 'CIntervalClass] [wmc deg] 
    (let [du (diat-up wmc deg)]
      (when-not (in? (:main-degrees wmc) du) du)))
  
  (b-meth aux-up ['WMode 'CIntervalClass] [wm deg] 
    (let [du (diat-up wm deg)]
      (when-not (in? (-> wm :w-mode-class :main-degrees) du) du)))
  
  (b-meth aux-up ['WMode 'PitchClass] [wm pc] 
    (let [du (diat-up wm pc)]
      (when-not (in? (:main-pitch-classes wm) du) du)))
  
  ;;;
  (b-multi aux-down
    "return the first adjacent down aux-degree/pitch-class or nil")
  
  (b-meth aux-down ['WModeClass 'CIntervalClass] [wmc deg] 
    (let [du (diat-down wmc deg)]
      (when-not (in? (:main-degrees wmc) du) du)))
  
  (b-meth aux-down ['WMode 'CIntervalClass] [wm deg] 
    (let [du (diat-down wm deg)]
      (when-not (in? (-> wm :w-mode-class :main-degrees) du) du)))
  
  (b-meth aux-down ['WMode 'PitchClass] [wm pc] 
    (let [du (diat-down wm pc)]
      (when-not (in? (:main-pitch-classes wm) du) du)))

  ;;;
  (b-fn auxs-up 
    "return a coll of aux-degrees or aux-pitch-classes 
    that are between target degree/pitchclass and first up main-degree/pitchclass"
    [wm-or-wmc deg-or-pc]
    (loop [deg-or-pc deg-or-pc
           ret []]
      (if-let [nxt (aux-up wm-or-wmc deg-or-pc)]
        (recur nxt (conj ret nxt))
        (when (seq ret) ret))))
  
  (b-fn auxs-down 
    "return a coll of aux-degrees or aux-pitch-classes 
    that are between target degree/pitchclass and first down main-degree/pitchclass"
    [wm-or-wmc deg-or-pc]
    (loop [deg-or-pc deg-or-pc
           ret []]
      (if-let [nxt (aux-down wm-or-wmc deg-or-pc)]
        (recur nxt (conj ret nxt))
        (when (seq ret) ret))))

