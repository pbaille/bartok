(in-ns 'bartok.primitives)

  (defn coerce-type [typ x]
    (let [litteral? (named? x)
          fast-parse (when litteral? (b> x))
          fast-match? (= typ (b-type fast-parse))
          pm (when (and litteral? (not fast-match?)) 
               (parses-map x))] 
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
            (if (match-type? typ x) 
              x
              (first (keep #(coerce-type % x) typ))))
        (and (uniform-collection-type? typ) (sequential? x))
          (let [ret (map (p coerce-type (first typ)) x)]
            (when (every? not-nil? ret)
              (if (= typ (do-type ['Any]))
                (when (uniform-collection-type? (b-type ret)) ret)  
                ret)))
        (and (collection-type? typ) (sequential? x))
          (let [ret (map coerce-type typ x)]
            (when (every? not-nil? ret) ret)))))
  
  (defn- coerce-types [type-vec argv]
    ; (dr)
    (let [ret (mapv coerce-type (do-types type-vec) argv)]
      (when (every? not-nil? ret) ret)))

  (comment 
    (pp (coerce-type (do-type #{'Mode :pitch}) "C1"))
    (pp (coerce-type (do-type [:pitch]) ["C1" "C2"]))
    (pp (coerce-types [:pitch [:alteration] 'Any] ["C1" [:# :b] :zoob]))
    (pp (coerce-type (do-type #{'String 'Keyword}) :zoob))
    (pp (coerce-type (do-type ['Any]) [:zoob :zer]))
    )
  
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
    (b-fun yo [:pitch p 'DIntervalClass di] (vector p di))
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
             (throw-type-exception ~argv-syms ~types-vec))))))
  
  (comment 
    (b-ufn yep [#{:pitch :pitchClass} aze ['DIntervalClass] [x & xs :as yop] 'Any & anys]
      (pp xs yop anys))
    
    (yep "C1" [:3rd "fourth" :5th] 12 "")
    
    (b-type (a b-types [:aze :zer :ert]))
    (= (do-types [:pitch ['DIntervalClass]]) (a b-types (coerce-types [:pitch ['DIntervalClass]] ["C1" [:3rd "fourth"]])))
  )
  
  (defn coerce-to-available-dispatch 
    "coerce args to the first possible dispatch of a multimethod
    and return a vector with the matching method and coerced args"
    [multi args]
    (let [meths (methods multi)
          cnt (count args)]
      ; (dr)
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
      (defmult ~nam b-types ~@tail)
      (defmethod ~nam :default [& args#] 
        (if-let [[fun# args#] (coerce-to-available-dispatch ~nam args#)]
          (a fun# args#)
          (throw (Exception. 
           (str "Bartok type error, can't find dispatch for " args#)))))))


(comment 
  (b-umulti yyl
    [:pitch p]
    (pp "pitch")
    [:pitch p1 :pitch p2]
    (pp "2 pitches!")
    [[:pitch] [pc1 pc2 & pcs]]
    (pp pc1 pc2 pcs)
    [[:pitch 'Any] pcs]
    (pp "pitchanyvec"))
  
  (b-mfun yip [#{:pitch :pitchClass} aze ['DIntervalClass] [x & xs :as yop]]
      (pp xs yop))
  
  (yip "C1" [:3rd "fourth" :5th])
  
  (yyl ["C1" "D1" "E1" "F#1"])
)