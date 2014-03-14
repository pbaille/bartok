(in-ns 'bartok.primitives)

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
      (simple-type t))
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

(declare types*)

(defn type* [x]
  (if-let [litteral-type (when-not (type? x) (b? x))]
    litteral-type
    (if (java-class-type? (type x))
     (cond
       (vector? x)
         (let [[ft :as bts] (a types* x)]
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
       (sequential? x) (type* (vec x)))
     (if (type? (type x))
       (type x)
       (when-let [t (type x)](simple-type t))))))

(comment 
  (type* '(:aze :aze :xcv)))

(defn types* 
  "ex: 
  (types* {:m2 'A :Lyd :4|4} (b> :m2) #{:C# :C#2})
  => [{:mode-class :time-signature, :c-interval-class :natural-pitch-class} 
      CIntervalClass 
      #{:pitch-class :pitch}]"
  [& xs] (mapv type* xs))

(defn match-type? [t x]
  (cond 
    (= t any-type) true  
    (= t (do-type ['Any])) 
      (uniform-collection-type? (type* x))
    (and (collection-type? t) (in? t any-type)) 
      (match-types? t x)
    (union-type? t) 
       (first-truthy #(match-type? % x) t)
    (= t (simple-type 'Vector)) (vector? x)
    :else (= t (type* x))))

(comment
  (match-type? (do-type #{'Foo 'Number}) 12)
  (match-type? (do-type 'Vector) [1 2]) 
  (match-type? (do-type ['Number]) [1 2]))

(defn match-types? [tps args]
  (every? (p a match-type?) (map vector tps args)))

;;; #clojure llasram's print method dispatch solution ;;;
  (defmulti print-method* (fn [x w] (type x)))
  (doseq [[dv m] (.getMethodTable print-method)] 
    (.addMethod print-method* dv m))
  (doseq [[dv dvs] (.getPreferTable print-method), dv' dvs] 
    (.preferMethod print-method* dv dv'))
  (.bindRoot #'print-method print-method*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn parses-map* [x] 
    ; (pp "zop")(dr)
    (reduce #(assoc %1 (first %2) (second %2)) 
            {} 
            (map-h #(hash-map %1 (first %2)) 
                   (group-by type* (b-parser (name x))))))

  (comment 
    (map type (keys (parses-map* "m3"))))
    
  (defn coerce-type [typ x]
    (let [destr? (or (map? x) (vector? x))
          litteral? (named? x)
          fast-parse (when litteral? (b> x))
          fast-match? (= typ (type* fast-parse))
          pm (when (and litteral? (not fast-match?)) 
               (parses-map* x))] 
      (cond 
        fast-match? fast-parse
        (any-type? typ) x
        (simple-type? typ) 
          (if (seq pm) 
            (get pm typ) 
            (when (match-type? typ x) x))
        (union-type? typ)
          (if (seq pm)
            (first (keep #(get pm %) typ))
            (when (match-type? typ x) x))
        (uniform-collection-type? typ)
          (let [ret (map (p coerce-type (first typ)) x)]
            (when (every? not-nil? ret)
              (if (= typ (do-type ['Any]))
                (when (uniform-collection-type? (type* ret)) ret)  
                ret)))
        (collection-type? typ)
          (let [ret (map coerce-type typ x)]
            (when (every? not-nil? ret) ret)))))
  
  (defn- coerce-types [type-vec argv]
    (let [ret (mapv coerce-type (do-types type-vec) argv)]
      (when (every? not-nil? ret) ret)))

  (comment 
    (pp (coerce-type (do-type #{'Mode 'Pitch}) "C1"))
    (pp (coerce-type (do-type ['Pitch]) ["C1" "C2"]))
    (pp (coerce-types ['Pitch ['Alteration] 'Any] ["C1" [:# :b] :zoob]))
    (pp (coerce-type (do-type #{'String 'Keyword}) :zoob))
    (pp (coerce-type (do-type ['Any]) [:zoob :zer]))
    )


  
;;;;;;;;;;;;;; macros ;;;;;;;;;;;;;;;;;;;
  
  (defn- restructuring-helper 
    "this is nescessary when destructuring is used, 
    because for coercing args can't use destructuring patterns
    it simply add `:as (gensym)` in destructuring stuff when missing"
    [argv]
    (reduce 
      (fn [acc arg]
        (cond 
         (map? arg)
           (if-let [as-sym (:as arg)]
             (-> acc 
              (update-in [:argv] conj arg)
              (update-in [:restruc-args] conj as-sym))
             (let [gs (gensym)]
               (-> acc
                (update-in [:argv] conj (assoc arg :as gs))
                (update-in [:restruc-args] conj gs))))
         (vector? arg)
           (let [idx (.indexOf arg :as)]
             (if (not= -1 idx)
               (-> acc 
                 (update-in [:argv] conj arg)
                 (update-in [:restruc-args] conj (nth arg (inc idx))))
               (let [gs (gensym)]
                 (-> acc
                  (update-in [:argv] conj (vec (concat arg [:as gs])))
                  (update-in [:restruc-args] conj gs)))))
         :else
         (-> acc 
          (update-in [:argv] conj arg)
          (update-in [:restruc-args] conj arg))))
      {:argv [] :restruc-args []}
      argv))
  
  (defn- rest-arg-helper 
    "manage rest argument and split types and args
    return [rest-arg? :boolean 
            types-vec :vector
            argv      :vector]"
    [args]
    (let [rest-arg? (= '& (first (take-last 2 args)))
          rest-sym (when rest-arg? (last args))
          args (if rest-arg? (butlast args) args)
          types-vec (vec (take-nth 2 args))
          argv (if-not rest-arg? 
                 (vec (take-nth 2 (next args)))
                 (conj (vec (take-nth 2 (next args))) rest-sym))]
      [rest-arg? types-vec argv]))
  
  (defn- throw-type-exception [args types]
    (throw (Exception. 
      (str "bartok type error... cannot coerce " 
           args " to " types))))
  
  (defmacro b-ufn 
    "define a bartok 'user' function:
    designed in order to provide some flexibility with litterals args
    the arg vector must be of the form [type-arg1 arg1-name type-arg2 arg2-name ...]
    if litterals are passed to this function, 
    they are automaticaly coerced to the good types if possible
    
    ex: 
    (b-fun yo ['Pitch p 'DIntervalClass di] (vector p di))
    "
    [nam & tail]
    (let [ds (when (string? (first tail)) (first tail))
          [args & body] (if ds (next tail) tail)
          [rest-arg? types-vec argv] (rest-arg-helper args)
          {:keys [argv restruc-args]} (restructuring-helper argv)
          argv-syms (vec (remove (p = '&) restruc-args))]
      `(defn ~nam ~argv 
         (if (match-types? (do-types ~types-vec) ~argv-syms)
           ~@body
           (if-let [args# (coerce-types ~types-vec ~argv-syms)]
             (a ~nam (if ~rest-arg? 
                       (concat (butlast args#) (last args#)) 
                       args#))
             (throw-type-exception ~argv-syms ~types-vec))))))))
  
  (comment 
    (b-ufn yep [#{'Pitch 'PitchClass} aze ['DIntervalClass] [x & xs :as yop] 'Any & anys]
      (pp xs yop anys))
    
    (yep "C1" [:3rd "fourth" :5th] 12 "")
    
    (type* (a types* [:aze :zer :ert]))
    (= (do-types ['Pitch ['DIntervalClass]]) (a types* (coerce-types ['Pitch ['DIntervalClass]] ["C1" [:3rd "fourth"]])))
  )
  
  (defn- coerce-to-available-dispatch 
    "coerce args to the first possible dispatch of a multimethod
    and return a vector with the matching method and coerced args"
    [multi args]
    (let [meths (methods multi)
          cnt (count args)]
      (first (keep #(when-let [args (coerce-types % args)]
                      (vector (get meths %) args)) 
                   (remove #(or (= :default %) (not= cnt (count %))) 
                           (keys meths))))))
  
  ;TODO add possibility to define :default dispatch 
  ;(need to combine with default behavior)
  (defmacro b-umulti 
    "like defmult macro but if no available dispatch try to coerce args"
    [nam & tail]
    `(do 
      (defmult ~nam types* ~@tail)
      (defmethod ~nam :default [& args#] 
        (if-let [[fun# args#] (coerce-to-available-dispatch ~nam args#)]
          (a fun# args#)
          (throw (Exception. 
           (str "Bartok type error, can't find dispatch for " args#)))))))


(comment 
  (b-umulti yyl
    ['Pitch p]
    (pp "pitch")
    ['Pitch p1 'Pitch p2]
    (pp "2 pitches!")
    [['Pitch] [pc1 pc2 & pcs]]
    (pp pc1 pc2 pcs)
    [['Pitch 'Any] pcs]
    (pp "pitchanyvec"))
  
  (b-mfun yip [#{'Pitch 'PitchClass} aze ['DIntervalClass] [x & xs :as yop]]
      (pp xs yop))
  
  (yip "C1" [:3rd "fourth" :5th])
  
  (yyl ["C1" "D1" "E1" "F#1"])
)
  
