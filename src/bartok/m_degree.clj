(ns bartok.m-degree
  (:use [bartok.constants])
  (:use [utils.utils]))

(defrecord MDegree [name dist generic])

(defn m-degree? [x] (= (class x) bartok.m_degree.MDegree))

;***************** Constructor *****************

(defmulti m-degree 
  (fn [& args]
    (case (count args)
      1 (let [f (first args)]       
          (cond 
            (m-degree-name? f)         :name 
            (m-degree-generic-name? f) :generic
            (number? f)                :number  ))
      
      2 (when (m-degree-generic-name? (first args)) [:generic :dist]))))

(defmethod m-degree :name [n] 
  (let [nam (keyword n)
        dist (m-degree-dist n)
        gen (m-degree-generic n)] 
    (->MDegree nam dist gen )))

(defmethod m-degree :generic [gen] 
  (let [nam (m-degree-generic-defaults (keyword gen)) 
        dist (m-degree-dist nam )] 
    (->MDegree nam dist gen )))

(defmethod m-degree :number [number] 
  (let [dist (mod number 12)
        nam (m-degree-dist-defaults dist)
        gen (m-degree-generic nam)] 
    (->MDegree nam dist gen)))

(defmethod m-degree [:generic :dist] [g dist] 
  (let [gen (keyword g)
        nam (get-in m-degrees [gen dist])]
    (->MDegree nam dist gen)))