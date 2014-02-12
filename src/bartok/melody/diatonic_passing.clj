(ns bartok.melody.diatonic-passing
  (:use [clojure.math.combinatorics :as c])
  (:use bartok.primitives)
  (:use bartok.types.w-mode)
  (:use utils.all))

(b-fn x-d-passing-context
  "return the passing environment of a degree/pitch-class in a w-mode(-class)"    
  [wm?c deg-pc]
  (let [cu (chromatic-up wm?c deg-pc)
        cd (chromatic-down wm?c deg-pc)
        [au1 au2] (auxs-up wm?c deg-pc)
        [ad1 ad2] (auxs-down wm?c deg-pc)
        mu (main-up wm?c deg-pc)
        md (main-down wm?c deg-pc)
        typ (if (type= deg-pc 'PitchClass) :pitch-class :degree)]
  [(:name deg-pc)
   {:chromatic-up   (when cu  {typ cu  :c-interval (c-interval deg-pc cu  :u)})
    :chromatic-down (when cd  {typ cd  :c-interval (c-interval deg-pc cd  :d)})
    :aux1-up        (when au1 {typ au1 :c-interval (c-interval deg-pc au1 :u)})
    :aux1-down      (when ad1 {typ ad1 :c-interval (c-interval deg-pc ad1 :d)})
    :aux2-up        (when au2 {typ au2 :c-interval (c-interval deg-pc au2 :u)})
    :aux2-down      (when ad2 {typ ad2 :c-interval (c-interval deg-pc ad2 :d)})
    :main-up        (when mu  {typ mu  :c-interval (c-interval deg-pc mu  :u)})
    :main-down      (when md  {typ md  :c-interval (c-interval deg-pc md  :d)})}]))

(b-multi d-passing-context
  "assign to each degree of a w-mode(-class) its passing environment")

(b-meth d-passing-context 'WMode [wm] 
  (entries->h-map (map (p x-d-passing-context wm) (:pitch-classes wm))))

(b-meth d-passing-context 'WModeClass [wmc] 
  (entries->h-map (map (p x-d-passing-context wmc) (:degrees wmc))))

(b-fn pitch-passings
  "return a collection of valid passings for a given target-pitch"
  [pass-cont target-pitch size]
  (let [context (dissoc-nils ((-> target-pitch :pitch-class :name) pass-cont))
        context (dissoc context :chromatic-down :chromatic-up)
        types (keys context)
        all (shuffle (c/selections types size))
        rep-rem (filter #(every? (fn [[x y]](not= x y)) (partition 2 1 %)) all)]
    (map (fn [pseq] 
           (reduce #(cons (transpose target-pitch (-> context %2 :c-interval)) %1) 
                   [target-pitch]
                   pseq)) 
         rep-rem)))

(comment 
  (map (fn [pp] (map :name pp)) 
       (pitch-passings (d-passing-context (w-mode :C-Lyd)) :B0 3)))

(b-fn make-passing 
  "temp fn to ear generated diatonic passings"
  [pass-cont target-pitch size brod?]
  (let [context (dissoc-nils ((-> target-pitch :pitch-class :name) pass-cont))
        context (dissoc context :chromatic-down :chromatic-up)
        types (keys context)
        sels (shuffle (c/selections types size))
        sel (select-first #(every? (fn [[x y]](not= x y)) (partition 2 1 %)) sels)
        final (reduce #(cons (transpose target-pitch (-> context %2 :c-interval)) %1) [target-pitch] sel)]
    final))
