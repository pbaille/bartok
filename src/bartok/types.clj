(in-ns 'bartok.primitives)

(defn- capitalized? [x]
  (re-matches #"[A-Z].+" (name x)))

(defn type? [x] (:is-type (meta x)))

(defn create-type [type x]
  (with-meta x {:type type :is-type true}))

(defn meta-type [x]
  (type (type x)))

(declare do-type)

(def  any-type 
  (create-type 'AnyType 'Any))
(defn simple-type [sym] 
  (create-type 'SimpleType sym))
(defn litteral-type [sym]
  (with-meta sym
    {:type 'SimpleType :is-type true :is-litteral true}))
(defn union-type [& types] 
  (create-type 'UnionType 
   (set (map do-type types))))
(defn uniform-collection-type [sym]
  (create-type 'UniformCollectionType 
    [(do-type sym)]))
(defn collection-type [& syms]
  (create-type 'CollectionType 
    (mapv do-type syms)))

(defn any-type? [t]
  (type= t 'AnyType))
(defn simple-type? [t] 
  (type= t 'SimpleType))
(defn litteral-type? [t]
  (and (simple-type? t)(:is-litteral (meta t))))
(defn union-type? [t] 
  (type= t 'UnionType))
(defn uniform-collection-type? [t]
  (type= t 'UniformCollectionType))
(defn collection-type? [t]
  (type= t 'CollectionType))

(defn java-class-type? [t]
  (= java.lang.Class (type t)))

(defn do-type [t]
  (cond 
   (type? t) t
   (symbol? t) 
    (if (= t 'Any) 
      any-type 
      ; if not capitalized symbol create litteral type
      (if (capitalized? t)
        (simple-type t)
        (litteral-type t)))
   (set? t) (a union-type t)
   (vector? t) 
    (if (count= t 1) 
     (uniform-collection-type (first t))
     (a collection-type t))))

(defn do-types [t-vec]
  (mapv do-type t-vec))

;certainly useless
(defn with-type* [type x]
  (with-meta x 
   {:type 
    (cond 
     (symbol? type) (simple-type type)
     (set? type) (union-type type)
     (vector? type) 
      (if (count= type 1) 
       (uniform-collection-type type)
       (collection-type type)))}))  

(declare b-types)

(defn b-type [x]
  (if-let [litteral (when-not (type? x) (b? x))]
    (litteral-type (symbol (name litteral)))
    (if (java-class-type? (type x))
     (cond
       (vector? x)
         (let [[ft :as bts] (a b-types x)]
           (if (all-eq? bts) 
             (uniform-collection-type ft) 
             (a collection-type bts)))
       (map? x)     (simple-type 'Map)
       (set? x)     (simple-type 'Set)
       (string? x)  (simple-type 'String)
       (keyword? x) (simple-type 'Keyword)
       (symbol? x)  (simple-type 'Symbol)
       (ratio? x)   (simple-type 'Ratio)
       (number? x)  (simple-type 'Number)
       (fn? x)      (simple-type 'Function)
       (sequential? x) (b-type (vec x)))
     (if (type? (type x))
       (type x)
       (when-let [t (type x)](simple-type t))))))

(comment 
  (b-type '(:aze :aze :xcv)))

(defn b-types 
  "ex: 
  (b-types {:m2 'A :Lyd :4|4} (b> :m2) #{:C# :C#2})
  => [{:mode-class :time-signature, :c-interval-class :natural-pitch-class} 
      CIntervalClass 
      #{:pitch-class :pitch}]"
  [& xs] (mapv b-type xs))

(declare match-types?)

(defn match-type? [t x]
  (cond 
    (= t any-type) true  
    (= t (do-type ['Any])) 
      (uniform-collection-type? (b-type x))
    (and (collection-type? t) (in? t any-type)) 
      (match-types? t x)
    (union-type? t) 
       (first-truthy #(match-type? % x) t)
    (= t (simple-type 'Vector)) (vector? x)
    :else (= t (b-type x))))

(comment
  (match-type? (do-type #{'Foo 'Number}) 12)
  (match-type? (do-type 'Vector) [1 2]) 
  (match-type? (do-type ['Number]) [1 2]))

(defn match-types? [tps args]
  (every? (p a match-type?) (map vector tps args)))



  
