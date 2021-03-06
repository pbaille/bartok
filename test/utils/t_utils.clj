(ns utils.t-utils
  (:use midje.sweet)
  (:require [utils.utils :refer :all]))


;***************** utils ********************
  
  (fact "or= works"
    (or= 12 1 3 5 78 12) => true
    (or= 12 1 3 5 78 11) => false)
     
  (fact "and= works"
    (and= (+ 1 12) 13 (+ 4 9)) => true
    (and= (+ 1 12) 13 (+ 4 10)) => false)
    
  (fact "and-not= works"
    (and-not= 12 1 3 5 78 11) => true
    (and-not= 12 1 3 5 78 (* 3 4)) => false)
  
  (fact "or-not= works"
    (or-not= 12 12 11 12 12) => true
    (or-not= 12 12 12) => false)

  (fact "or-> works"
    (or-> 12 (= 12) (> 11)) => true
    (or-> 12 (= 11) (> 11)) => true
    (or-> 12 (> 12) (= 1)) => false)
  
  
;********** strings and keywords ************
 
  (fact "named? works"
    (named? 'aze) => true
    (named? :az) => true
    (named? "az") => true
    (named? 1) => false
    (named? {:a 1}) => false
    (named? [1 2]) => false)
  
  (facts "about keyword-cat"
    (fact "it concat two or more keywords"
      (keyword-cat :foo :bar) => :foobar
      (keyword-cat :foo :bar :aze) => :foobaraze)
    (fact "it concat keywords with strings"
      (keyword-cat :foo "bar") => :foobar
      (keyword-cat "foo" :bar :aze) => :foobaraze)
    (fact "it concat symbols keywords strings and ignore nil"
      (keyword-cat :foo "b" nil 'ar) => :foobar
      (keyword-cat nil :foo 'bar "aze" 1) => :foobaraze1))

 
;   (defn dash-split [x] 
;     (when (named? x) (clojure.string/split (name x) #"\-")))
  
;   (defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))
;   (defn kw->str [kw] (-> kw str (subs 1)))
;   (defn kw-or-str? [x] (or (keyword? x) (string? x)))

; ;**************** numbers *******************

;   (defn mod12 [x] (mod x 12))
;   (defn mod12map [coll] (map mod12 coll))
;   (defn abs [x] (if (neg? x) (* x -1) x))
;   (defn rand-int-between [a b] (rand-nth (range a b)))
;   (defn div-mod [x div] [(int (/ x div)) (mod x div)])
;   (defn int-div [x div] (int (/ x div)))
;   (defn median [& args] (/ (apply + args) (count args)))
  
;   (defn round [s n] 
;     (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)) 
  
;   (defn between
;     ([a b] (between a (first b) (second b)))
;     ([a b1 b2] (and (>= a b1) (<= a b2))))
  
;   (defn prime-factors [n]
;     (loop [n n divisor 2 factors []]
;       (if (< n 2)
;         factors
;         (if (= 0 (rem n divisor))
;           (recur (/ n divisor) divisor (conj factors divisor))
;           (recur n (inc divisor) factors)))))
  
;   (defn steps 
;     "return steps between each adjacent items of coll"
;     [coll] 
;     (reduce #(conj %1 (apply - (reverse %2))) [] (partition 2 1 coll)))
  
;   ;from overtone
;   (defn scale-range
;     "Scales a given input value within the specified input range to a
;     corresponding value in the specified output range using the formula:
  
;              (out-max - out-min) (x - in-min)
;      f (x) = --------------------------------  + out-min
;                       in-max - in-min
  
;   "
;     [x in-min in-max out-min out-max]
;     (+ (/ (* (- out-max out-min) (- x in-min))
;           (- in-max in-min))
;        out-min))
  
  
;   (defn range-scaler [min-in max-in min-out max-out]
;     #(scale-range % min-in max-in min-out max-out))
  
;     ; max inclusive
;   (defn range-by 
;     ([end step] (range-by 0 end step))
;     ([start end step] 
;       (let [incf (if (> start end) - + )
;             amp (math/abs (- end start))]
;         (map #(-> (* % step) incf (+ start)) 
;            (range 0 ((c inc int) (/ amp step)))))))
  
;   ;(range-by 10 0.25)

; ;***************** colls ********************

;   (defn rotate [coll n]
;     (let [c (count coll)
;           n (if (>= n 0) (mod n c) (+ n (count coll)))
;           splited (split-at n coll)]
;       (concat (splited 1) (splited 0))))
  
;   (defn in? 
;     "true if seq contains elm"
;     [seq elm]  
;     (some #(= elm %) seq))
  
;   (defn indexed [coll]
;     (cond
;       (map? coll) (seq coll)
;       (set? coll) (map vector coll coll)
;       :else (map vector (iterate inc 0) coll)))
  
;   (defn pos [pred coll]
;     (for [[i v] (indexed coll) :when (pred v)] i))
  
;   (defn index-of [item coll] (first (pos #{item} coll)))
  
;   (defn count= [coll c] (-> coll count (= c)))
  
;   (defn fill-with [coll size el]
;     (let [tail (repeat (- size (count coll)) el)]
;       (concat coll tail)))
  
; ;***************** maps *********************
  
;   (def h-map hash-map)
  
;   (defn tups->h-map 
;     "(tups->h-map [[:a 1][:b 2]]) => {:a 1 :b 2}"
;     [tups-coll]
;     (a hash-map (a concat tups-coll)))
  
;   (defn dissoc-in [m key-vec]
;     (let [firsts (vec (butlast key-vec))
;           node (dissoc (get-in m firsts) (last key-vec))]
;       (assoc-in m firsts node)))
  
;   (defn submap? [sub m] 
;     (clojure.set/subset? (set sub) (set m)))
  
;   ; find the first value for kw key in map or nested maps 
;   (defn in> [m kw ]
;     (when (map? m)
;       (if-let [v (kw m)] 
;         v (in> (apply merge (filter map? (vals m))) kw))))
  
;   (defn map-vals [f m]
;     (a merge (map (fn [[k v]] {k (f v)}) m)))
  
;   (defn map-h  [f m]
;     {:pre [(map? m)]}
;     (let [hms (map (fn [[k v]] (f k v)) m)]
;       (if (second hms) (a conj hms) (first hms))))
  
;   ;(map-h (fn [k v] {k (inc v)}) {:a 1 :b 2})
;   ;=> {:a 2 :b 3}

  (fact "key-path"
    (key-path :a {:aze {:b 1} :bifl {:qsd {:cv 23 :m {:z 1 :a 345} } :q 1}}) 
      => [:bifl :qsd :m :a]
    (key-path :a {:aze {:a 1} :bifl {:qsd {:cv 23 :m {:z 1 :a 345} } :q 1}}) 
      => [:aze :a])

; ;**************** vectors *******************

;   (defn vec-if-not [x]
;     (if (vector? x) x (vector x)))
  
;   ; comparators

;   (defn v-eq [v1 v2]
;     (= 0 (compare v1 v2)))
  
;   (defn v-lt [v1 v2]
;     (= -1 (compare v1 v2)))
  
;   (defn v-gt [v1 v2]
;     (= 1 (compare v1 v2)))
  
;   (defn v-gte [v1 v2]
;     (let [c (compare v1 v2)]
;       (or (= 0 c) (= 1 c))))
  
;   (defn v-lte [v1 v2]
;     (let [c (compare v1 v2)]
;       (or (= 0 c) (= -1 c))))
   
; ;*************** functions ******************
  
;   ;shortcuts
;   (def p partial)
;   (def c comp)
;   (def a apply)
  
;   (defn ap [f & args]
;     (apply (apply partial f (butlast args)) (last args)))
;   ;(ap + 2 3 [1 2 3 4])
  
  
;   (defn call [^String nm & args]
;       (when-let [fun (ns-resolve *ns* (symbol nm))]
;           (apply fun args)))
  
; ;************ higher order funs *************
  
;   ; (defn map-reduce-old [f init coll] 
;   ;   ((c vec next) 
;   ;      (reduce (fn [acc el] 
;   ;                (conj acc (f (last acc) el))) 
;   ;               [init] 
;   ;               coll)))
  
;   (defn map-reduce [f init coll] 
;     (next (reductions f init coll)))
  
;   (defn map-with-coll [f coll]
;     (map f coll (repeat coll)))
  
;   ; takes function that takes coll as third argument
;   (defn red-with-coll [f init coll] 
;     (reduce #(f %1 %2 coll) init coll))
  
;   (defn map-with-index [f coll]
;     (map f coll (range)))
  
;   (defn best [f coll]
;     (reduce #(if (f %1 %2) %1 %2) coll))
  
;   (defn select-first [pred coll]
;     (first (filter pred coll)))
  
;   ;map with f and remove falsy vals
;   (defn filt-map [f coll]
;     (filter (complement nil?) (map f coll)))
  
;   ;(filt-map #(when (< 0 %) %) [-1 2 3])
;   ;=> (2 3)
  
;   ;like filt-map but just first
;   (defn first-truthy [f coll]
;     (select-first (complement nil?) (map f coll)))
  
;   ;(first-truthy #(when (< 0 %) %) [-1 2 3])
;   ;=> 2
  
;   (defn set-map [f coll]
;     (set (map f coll)))
  
  
  
;   ; (defn first-where [sub-map coll]
;   ;   (select-first #(submap? sub-map %) coll))
  
;   ; (defn select-where [sub-map coll]
;   ;   (filter #(submap? sub-map %) coll))
  
; ;***************** types ********************
  
;   (defn type= [obj type-sym]
;     (= (type obj) type-sym))
  
  (fact "same-type"
    (same-type? 1 2) => true 
    (a same-type? (range 12)) => true)
    
;   (defn types 
;     ([arg] (type arg))
;     ([arg & more] (vec (map type (concat [arg] more)))))
  
;   (defn with-type [t obj]
;     {:pre [(named? t)]}
;     (when obj (with-meta obj {:type t})))

; ;*************** experiments ****************

;   (defn repeater [coll]
;     (mapcat (fn [[n & els]] 
;               (if (count= els 1)
;                 (repeat n (first els)) 
;                 (a concat (repeat n (mapcat #(repeater [%]) els))))) 
;             (map #(if (vector? %) % (vector 1 %)) coll)))
  
;   ;(repeater [[2 [3 [2 :foo] :bar] :woz] :bim [2 :yo]])
;   ;=> (:foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :bim :yo :yo)
  
; ;************** namespaces ******************

;   ; https://groups.google.com/forum/#!topic/clojure/GAGF38uI1-o
;   ; by James reeves

;   (defn- merge-meta! 
;     "Destructively merge metadata from a source object into a target." 
;     [source target] 
;     (.setMeta target 
;       (merge (meta source) 
;              (select-keys (meta target) [:name :ns])))) 
  
;   (defn immigrate 
;     "Add all the public vars in a list of namespaces to the current 
;   namespace." 
;     [& namespaces] 
;     (doseq [ns namespaces] 
;       (require ns) 
;       (doseq [[sym v] (ns-publics (find-ns ns))] 
;         (merge-meta! v 
;           (if (.isBound v) 
;             (intern *ns* sym (var-get v)) 
;             (intern *ns* sym)))))) 

; ;stuart sierra version

; ; (defn immigrate
; ;   "Create a public var in this namespace for each public var in the
; ;   namespaces named by ns-names. The created vars have the same name, root
; ;   binding, and metadata as the original except that their :ns metadata
; ;   value is this namespace."
; ;   [& ns-names]
; ;   (doseq [ns ns-names]
; ;     (require ns)
; ;     (doseq [[sym var] (ns-publics ns)]
; ;       (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
; ;         (if (.hasRoot var)
; ;           (intern *ns* sym (.getRoot var))
; ;           (intern *ns* sym))))))
