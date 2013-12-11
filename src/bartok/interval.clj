(ns bartok.interval
  (:use [bartok.litterals.identity])
  (:use [bartok.interval-class])
  (:use [utils.utils]))

(def directions {:u 1 :d -1})

(def intervals 
  (reduce conj #{}      
    (for [{icn :name icv :val icg :generic :as ic} interval-classes 
           oct (range 8)
          [dirn dirv] directions]      
      {:name (keyword (str (name icn) (name dirn) (if (= 0 oct) "" oct)))
       :val (* dirv (+ icv (* 12 oct)))
       :direction {:name dirn :val dirv}
       :octave-offset oct
       :interval-class (map->IntervalClass ic)})))

(def name->interval (reduce #(into %1 {(:name %2) %2}) {} intervals))

(def val->interval 
  (reduce #(into %1 {(:val %2) %2}) {} 
          (filter #(interval-class-default-names (get-in % [:interval-class :name])) 
                  intervals)))

(defrecord Interval [name val direction octave-offset interval-class])

(defn map->Interval [m] (->Interval (:name m) (:val m) (:direction m) (:octave-offset m) (:interval-class m)))

(defmulti interval
  (fn [arg]
    (cond
      (interval-name? arg) :name
      (interval-class-name? arg) :interval-class
      (generic-interval-class-name? arg) :generic
      (number? arg) :val)))

(defmethod interval :name [n] (map->Interval (name->interval n)))
(defmethod interval :interval-class [ic] (map->Interval (name->interval (keyword-cat ic :u))))
(defmethod interval :generic [g] 
  (map->Interval 
    (name->interval 
      (keyword-cat (:name (generic->default-interval-class g)) 
                    :u))))
(defmethod interval :val [v] (map->Interval (val->interval v)))







; (defrecord Interval [name size direction])

; ;************* Constructor ****************

; (defmulti interval 
;   (fn [& args]
;     (let [nam  (select-first m-degree-name? args)
;           gen  (select-first m-degree-generic-name? args)
;           size (select-first number? args)
;           oct-add (some :oct-add args)
;           dir (select-first direction? args)]
;       (cond
;         (and gen size dir oct-add) 
;           [:generic :size :direction :oct-add ]
;         (and gen size dir)
;           [:generic :size :direction]
;         (and nam oct-add dir)
;           [:name :direction :oct-add]
;         (and gen size oct-add)
;           [:generic :size :oct-add]
          
;         (and nam oct-add) [:name :oct-add]
;         (and nam dir)     [:name :direction]
;         (and gen size)    [:generic :size]
;         (and gen dir)     [:generic :direction]
        
;         nam   :name 
;         gen   :generic
;         size  :number ))))


; (defmethod interval [:generic :size :direction :oct-add] [gen size dir oa]
;   (let [nam (get-in m-degrees [gen (mod12 size)])
;         size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
;     (->Interval nam size dir)))

; (defmethod interval [:generic :size :direction] [gen size dir]
;   (let [nam (get-in m-degrees [gen (mod12 size)])]
;     (->Interval nam size dir)))

; (defmethod interval [:name :direction :oct-add ] [nam dir oa]
;   (let [size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
;     (->Interval nam size dir)))

; (defmethod interval [:generic :size :oct-add] [gen size oa]
;   (let [nam (get-in m-degrees [gen (mod12 size)])
;         size (+ size (* (:oct-add oa) 12))]
;     (->Interval nam size :up)))

; (defmethod interval [:name :oct-add] [nam oa] 
;   (let [size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
;     (->Interval nam size :up)))

; (defmethod interval [:name :direction] [nam dir] 
;   (let [size (m-degree-dist nam)]
;     (->Interval nam size dir)))

; (defmethod interval [:generic :size] [gen size] 
;   (let [ nam (get-in m-degrees [gen (mod12 size)])
;          dir (if (neg? size) :down :up)]
;     (->Interval nam size dir)))

; (defmethod interval [:generic :direction] [gen dir] 
;   (let [nam (m-degree-generic-defaults gen)
;         size (m-degree-dist nam)]
;     (->Interval nam size dir)))
      
; (defmethod interval :name [n] 
;   (let [nam (keyword n)
;         size (m-degree-dist n)
;         gen (m-degree-generic n)] 
;     (->Interval nam size :up )))

; (defmethod interval :generic [gen] 
;   (let [nam (m-degree-generic-defaults (keyword gen)) 
;         size (m-degree-dist nam )] 
;     (->Interval nam size :up )))

; (defmethod interval :number [number] 
;   (let [dir (if (neg? number) :down :up)
;         size (abs number)
;         nam (m-degree-dist-defaults (mod12 size))] 
;     (->Interval nam size dir)))

; ; (defmethod interval :degrees [degrees]
; ;   (let [size ()
; ;         nam (m-degree-dist-defaults (mod12 size))] 
; ;     (->Interval nam size :up)))
       


