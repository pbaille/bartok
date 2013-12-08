(ns bartok.interval
  (:use [bartok.constants])
  (:use [bartok.m-degree])
  (:use [utils.utils]))

(defrecord Interval [name size direction])

;************* Constructor ****************

(defmulti interval 
  (fn [& args]
    (let [nam  (select-first m-degree-name? args)
          gen  (select-first m-degree-generic-name? args)
          size (select-first number? args)
          oct-add (some :oct-add args)
          dir (select-first direction? args)]
      (cond
        (and gen size dir oct-add) 
          [:generic :size :direction :oct-add ]
        (and gen size dir)
          [:generic :size :direction]
        (and nam oct-add dir)
          [:name :direction :oct-add]
        (and gen size oct-add)
          [:generic :size :oct-add]
          
        (and nam oct-add) [:name :oct-add]
        (and nam dir)     [:name :direction]
        (and gen size)    [:generic :size]
        (and gen dir)     [:generic :direction]
        
        nam   :name 
        gen   :generic
        size  :number ))))


(defmethod interval [:generic :size :direction :oct-add] [gen size dir oa]
  (let [nam (get-in m-degrees [gen (mod12 size)])
        size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
    (->Interval nam size dir)))

(defmethod interval [:generic :size :direction] [gen size dir]
  (let [nam (get-in m-degrees [gen (mod12 size)])]
    (->Interval nam size dir)))

(defmethod interval [:name :direction :oct-add ] [nam dir oa]
  (let [size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
    (->Interval nam size dir)))

(defmethod interval [:generic :size :oct-add] [gen size oa]
  (let [nam (get-in m-degrees [gen (mod12 size)])
        size (+ size (* (:oct-add oa) 12))]
    (->Interval nam size :up)))

(defmethod interval [:name :oct-add] [nam oa] 
  (let [size (+ (m-degree-dist nam) (* (:oct-add oa) 12))]
    (->Interval nam size :up)))

(defmethod interval [:name :direction] [nam dir] 
  (let [size (m-degree-dist nam)]
    (->Interval nam size dir)))

(defmethod interval [:generic :size] [gen size] 
  (let [ nam (get-in m-degrees [gen (mod12 size)])
         dir (if (neg? size) :down :up)]
    (->Interval nam size dir)))

(defmethod interval [:generic :direction] [gen dir] 
  (let [nam (m-degree-generic-defaults gen)
        size (m-degree-dist nam)]
    (->Interval nam size dir)))
      
(defmethod interval :name [n] 
  (let [nam (keyword n)
        size (m-degree-dist n)
        gen (m-degree-generic n)] 
    (->Interval nam size :up )))

(defmethod interval :generic [gen] 
  (let [nam (m-degree-generic-defaults (keyword gen)) 
        size (m-degree-dist nam )] 
    (->Interval nam size :up )))

(defmethod interval :number [number] 
  (let [dir (if (neg? number) :down :up)
        size (abs number)
        nam (m-degree-dist-defaults (mod12 size))] 
    (->Interval nam size dir)))

; (defmethod interval :degrees [degrees]
;   (let [size ()
;         nam (m-degree-dist-defaults (mod12 size))] 
;     (->Interval nam size :up)))
       


