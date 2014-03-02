(ns utils.utils
  (:use bartok.print)
  (:require vendors.debug-repl)
  (:require [clojure.inspector :refer [inspect-tree]])
  (:require [clojure.contrib.math :as math]))

(declare p a ap c)

;***************** utils ********************

  (def not-nil? (complement nil?))
  
  (defn pev [x] (do (pp x) x))
  
  (defmacro dr 
    ([] `(vendors.debug-repl/debug-repl))
    ([& args] `(do (vendors.debug-repl/debug-repl) ~@args )))
  
  (defmacro or-dr 
    "try an expr and launch debug-repl if Exception"
    [expr] 
    (let [line (:line (meta &form))
          file *file*]
      `(try ~expr (catch Exception e# (do (println (str "file:" ~file "::" ~line))(dr))))))
  
  (defmacro wai? 
    "where am I ? => print file and line"
    []
    `(println (str "file:" ~*file* "::" ~(:line (meta &form)))))
  
  (defmacro src 
    "print source :)"
    [x] 
    `(do (pp (:file (meta (resolve '~x))))(clojure.repl/source ~x)))
  
  ;inspect-tree shortcut
  (defn tree [x] (inspect-tree x))
  
  (defn pex 
    "prettyprint macro expansion"
    [expr] 
    (pp (macroexpand-1 expr)))
  
  ;this should be a macro no? expr is eval before entering this...
  (defmacro with-dispatch [disp-val expr]
    "call a particular dispatch on a multi method"
    `((get (methods ~(first expr)) ~disp-val) ~@(next expr)))
  
;********** strings and keywords ************

  (defn named? [x]
    (or (keyword? x) (string? x) (symbol? x)))
  
  (defn dash-split [x] 
    (when (named? x) (clojure.string/split (name x) #"\-")))
  
  (defn keyword-cat [& args] 
    (->> args
         (remove nil?)
         (map #(if (isa? (type %) java.lang.Number) (str %) %))
         (map name)
         (a str)
         keyword))
  
  (def kwcat keyword-cat)
  
  (defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))
  (defn kw->str [kw] (-> kw str (subs 1)))
  (defn kw-or-str? [x] (or (keyword? x) (string? x)))

;**************** numbers *******************

  (defn mod12 [x] (mod x 12))
  (defn mod12map [coll] (map mod12 coll))
  (defn abs [x] (if (neg? x) (* x -1) x))
  (defn rand-int-between [a b] (rand-nth (range a b)))
  (defn div-mod [x div] [(int (/ x div)) (mod x div)])
  (defn int-div [x div] (int (/ x div)))
  (defn same-sign? [x y] (pos? (* x y)))
  (defn dist [x y] (abs (- x y)))
  
  (defn median 
    ([coll] (/ (reduce + coll) (count coll)))
    ([x & xs] (median (cons x xs))))
  
  (defn opposite-sign? [x y] 
    (or (and (pos? x) (neg? y)) 
        (and (neg? x) (pos? y))))
  
  (defn round 
    ([n] (math/round n))
    ([s n] (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN))) 
  
  (defn between
    ([a b] (between a (first b) (second b)))
    ([a b1 b2] (and (>= a b1) (<= a b2))))
  
  (defn prime-factors [n]
    (loop [n n divisor 2 factors []]
      (if (< n 2)
        factors
        (if (= 0 (rem n divisor))
          (recur (/ n divisor) divisor (conj factors divisor))
          (recur n (inc divisor) factors)))))
  
  (defn steps 
    "return steps between each adjacent items of coll"
    [coll] 
    (reduce #(conj %1 (apply - (reverse %2))) [] (partition 2 1 coll)))
  
  (defn steps-bounds
    "return min and max amplitude of step-sequence"
    [steps]
    (let [mr (reductions + 0 steps)]
      [(a min mr)(a max mr)]))
  
  ;from overtone
  (defn scale-range
    "Scales a given input value within the specified input range to a
    corresponding value in the specified output range using the formula:
  
             (out-max - out-min) (x - in-min)
     f (x) = --------------------------------  + out-min
                      in-max - in-min
    "
    [x in-min in-max out-min out-max]
    (+ (/ (* (- out-max out-min) (- x in-min))
          (- in-max in-min))
       out-min))
  
  (defn range-scaler [min-in max-in min-out max-out]
    #(scale-range % min-in max-in min-out max-out))
  
  (defn range-by 
    "max inclusive"
    ([end step] (range-by 0 end step))
    ([start end step] 
      (let [incf (if (> start end) - + )
            amp (math/abs (- end start))]
        (map #(-> (* % step) incf (+ start)) 
           (range 0 ((c inc int) (/ amp step)))))))
  
  (defn min-max 
    "takes a seq of numbers and return a vector [min max]"
    [coll] 
    ((juxt #(a min %) #(a max %)) coll))
  
  (declare best)
  (defn closest 
    "takes a target (num) and a coll [(num)]
    returns the closest to target elem of coll"
    [target num-coll]
    (best #(< (dist target %1)(dist target %2)) num-coll))
  
  ;(range-by 10 0.25)

;***************** colls ********************

  (defn rotate [coll n]
    (let [c (count coll)
          n (if (>= n 0) (mod n c) (+ n (count coll)))
          splited (split-at n coll)]
      (concat (splited 1) (splited 0))))
  
  (defn in? 
    "true if seq contains elm"
    [seq elm]  
    (some #(= elm %) seq))
  
  (defn indexed [coll]
    (cond
      (map? coll) (seq coll)
      (set? coll) (map vector coll coll)
      :else (map vector (iterate inc 0) coll)))
  
  (defn pos [pred coll]
    ; (dr)
    (for [[i v] (indexed coll) :when (pred v)] i))
  
  (defn index-of [item coll] (first (pos #{item} coll)))
  
  (defn count= [coll c] (-> coll count (= c)))
  
  (defn fill-with [coll size el]
    (let [tail (repeat (- size (count coll)) el)]
      (into coll tail)))
  
  (defn seq1 
    "force 1 by one evaluation  of lazy sequence s"
    [s]
    (lazy-seq
      (when-let [[x] (seq s)]
        (cons x (seq1 (rest s))))))
  
  (defn partition-if
    "split coll where (pred elem next-elem) is true
    TODO good implementation :) "
    [pred coll]
    (lazy-seq
     (when (second coll)
       (let [temp (take-while 
                    #(not (pred (first %) (second %))) 
                    (partition 2 1 coll))
             lst (if (seq temp) (last (last temp)) (first coll))
             run (concat (mapv first temp) [lst])
             nxt (seq (drop (count run) coll))
             cnxt (count nxt)]
         (cond
           (>= cnxt 2) (cons run (partition-if pred nxt))
           (= cnxt 1)  (list run (list (last coll)))
           :else       (list run))))))

  (defn tails 
    "like haskell's Data.List tails
    ex: 
    (tails [1 2 3 4])
    => ([1 2 3 4] [2 3 4] [3 4] [4] [])"
    [xs]
    (let [f (cond
             (vector? xs) vec
             (set? xs) set
             :else sequence)]
      (map f (take (inc (count xs)) (iterate rest xs)))))

  (defn inits 
    "like haskell's Data.List tails
    ex:
    (inits [1 2 3 4])
    => (() (1) (1 2) (1 2 3))"
    [xs]
    (for [n (range (count xs))]
      (take n xs)))
  
  (defn all-distinct? 
    "true if coll contains only distinct values"
    [coll]
    (if (seq coll) (a distinct? coll) true))
  
  (defn all-eq?
    "true if all item of coll are eq"
    [coll]
    (= 1 (count (set coll))))

;***************** maps *********************
  
  (defn map->sorted 
    "given a hash-map return the corresponding sorted-map"
    [m] (a sorted-map (a concat m)))
  
  (defn sorted-map-by* 
    "given a hash-map and a function return the corresponding sorted-map-by
    the advantage over regular sorted-map-by is that f takes keyvals instead of keys"
    ([f m] (a sorted-map-by 
              #(f [%1 (%1 m)][%2 (%2 m)])
              (a concat m)))
    ; sorted by result of (ctor (f val1)(f val2))
    ([ctor f m] (a sorted-map-by 
                   #(comparator (f (%1 m))(f (%2 m)))
                   (a concat m))))
  
  (defn if-nil-merge [m1 m2]
    (merge-with #(if (or %1 %2) (if %1 %1 %2) nil) m1 m2))
  
  (def h-map hash-map)
  
  (defn entries->h-map 
    "(entries->h-map [[:a 1][:b 2]]) => {:a 1 :b 2}"
    [tups-coll]
    (a hash-map (a concat tups-coll)))
  
  (defn dissoc-in [m key-vec]
    (let [firsts (vec (butlast key-vec))
          node (dissoc (get-in m firsts) (last key-vec))]
      (assoc-in m firsts node)))
  
  (defn submap? [sub m] 
    (clojure.set/subset? (set sub) (set m)))
  
  (defn in> 
    "find the first value for kw key in map or nested maps"
    [m kw ]
    (when (map? m)
      (if-let [v (kw m)] 
        v (in> (apply merge (filter map? (vals m))) kw))))
  
  (defn map-vals [f m]
    (a merge (map (fn [[k v]] {k (f v)}) m)))
  
  (defn map-keys [f m]
    (a merge (map (fn [[k v]] {(f k) v}) m)))
  
  (defn map-h  
    "map over an hash-map and return a new hash-map
    f must return either a single keyval map or a duplet (vec of size 2)"
    [f m]
    {:pre [(map? m)]}
    (let [hms (map (fn [[k v]] (f k v)) m)]
      (if (vector? (first hms)) 
        (entries->h-map hms) 
        (reduce conj {} hms))))
  
  (defn map-h*
    "same as map-h but f is single arity and is applied to each key and val
    (map-h* inc {1 2 3 4}) <=> (map-h #(h-map (inc %) (inc %2)) {1 2 3 4})"
    [f m]
    (map-h (fn [k v] (vector (f k) (f v))) m))
  
  (defn dissoc-nils 
    "remove keyvals whose val is nil from a h-map"
    [m]
    (when m (map-h (fn [k v] (if v {k v} {})) m)))
  
  (declare first-truthy type=)
  
  (defn key-path
    "find the deepless path of given key in a map" 
    ([k coll] (key-path k [] coll))
    ([k pathv coll] 
      (if (get-in coll (conj pathv k)) 
        (conj pathv k) 
        (first-truthy 
          #(when (type= % clojure.lang.MapEntry) 
              (when (map? (val %)) (key-path k (conj pathv (key %)) coll))) 
          (get-in coll pathv)))))
  
  (defn filt-h 
    "like filter but takes and return hash-map
     if pred fail on a keyval it is dissoc from h"
    [pred h]
    (reduce (fn [acc el]
              (if-not (pred el)
                (dissoc acc (key el))
                acc)) 
            h 
            h))
  
  (defn dissoc-if 
    "same as filt-h but with args in reverse order to match with dissoc args order"
    [h pred] (filt-h pred h))

  (defn rem-h [pred h]
    "like remove but takes and return h-map (see filt-h)"
    (filt-h (complement pred) h))

;**************** vectors *******************

  (defn vec-if-not [x]
    (if (vector? x) x (vector x)))
  
  ; comparators

  (defn v-eq [v1 v2]
    (= 0 (compare v1 v2)))
  
  (defn v-lt [v1 v2]
    (= -1 (compare v1 v2)))
  
  (defn v-gt [v1 v2]
    (= 1 (compare v1 v2)))
  
  (defn v-gte [v1 v2]
    (let [c (compare v1 v2)]
      (or (= 0 c) (= 1 c))))
  
  (defn v-lte [v1 v2]
    (let [c (compare v1 v2)]
      (or (= 0 c) (= -1 c))))
   
;*************** functions ******************
  
  ;shortcuts
  (def p partial)
  (def c comp)
  (def a apply)
  
  (defn ap [f & args]
    "apply last argument to (partial f (butlast args))"
    (apply (apply partial f (butlast args)) (last args)))
  ;(ap + 2 3 [1 2 3 4])
  
  
  (defn call [^String nm & args]
    "call a function by string name 
    ex: (\"+\" 2 3)"
    (when-let [fun (ns-resolve *ns* (symbol nm))]
      (apply fun args)))
  
;************ higher order funs *************
  
  (defn map-reduce [f init coll] 
    (next (reductions f init coll)))
  
  (defn map-with-coll [f coll]
    "like map but f takes extra argument coll"
    (map f coll (repeat coll)))
  
  (defn red-with-coll [f init coll] 
    "same as reduce but takes a function that takes 3 arguments [acc el coll]"
    (reduce #(f %1 %2 coll) init coll))
  
  (defn best [f coll]
    (reduce #(if (f %1 %2) %1 %2) coll))
  
  (defn select-first [pred coll]
    (first (filter pred coll)))
  
  ;obsolete use clojure.core/keep instead of that
  ; (defn remnil-map [f coll]
  ;   "map coll with f and filter nils
  ;   (remnil-map #(when (< 0 %) %) [-1 2 3])
  ;   => (2 3)"
  ;   (remove nil? (map f coll)))
  
  (defn map-nth 
    "apply f on each nth elems of coll"
    [n f coll]
    (mapcat #(ap vector (f (first %)) (next %)) 
            (partition n n nil coll)))
  
  (defn filt-map [ff mf coll]
    "map coll with mf then filter results with ff"
    (filter ff (map mf coll)))
    
  (defn map-filt [mf ff coll]
    "filter coll with ff then map results with mf"
    (map mf (filter ff coll)))
  
  (defn first-truthy [f coll]
    "take the first truthy element of (map f coll)
    (first-truthy #(when (< 0 %) %) [-1 2 3])
    => 2"
    (select-first (complement nil?) (map f coll)))
  
  (defn set-map [f coll]
    "call set on the result of map"
    (set (map f coll)))
  
  (defn drop-last-while 
    "drop last items until pred is true
    vector only!
    (drop-last-while (p < 10) [57 8 1 6 80 9 90 99 78])
    => (57 8 1 6 80 9) "
    [pred coll]
    (let [cnt (count (drop-while pred (reverse coll)))]
      (take cnt coll)))
  
  (defn reduce-while 
    "reduce coll while (pred acc) 
    returns the last valid intermediate value of acc"
    [f pred init coll]
    (if (empty? coll)
      init
      (let [result (f init (first coll))]
        (if (pred result)
          (recur f pred result (rest coll))
          init))))
  
  (defn iterate-while
    "like (last (take-while pred (iterate f init))) "
    [pred f init]
    (let [result (f init)]
      (if (pred result)
        (recur pred f result)
        init)))

  (defn reduce-while-not-nil [f init coll]
    (reduce-while f not-nil? init coll))
  
;***************** types ********************
  
  (defn of-type? [obj typ]
    (isa? (type obj) typ))
  
  (defn type= [obj type-sym]
    (= (type obj) type-sym))
  
  (defn types 
    ([arg] (type arg))
    ([arg & more] (vec (map type (concat [arg] more)))))
  
  (defn same-type? 
    ([x1 x2] (= (type x1) (type x2)))
    ([x1 x2 & xs] 
      (and (same-type? x1 x2) 
        (if (seq (next xs))
          (a same-type? x2 (first xs) (next xs))
          (same-type? x2 (first xs))))))
  
  (defn with-type [t obj]
    {:pre [(named? t)]}
    (when obj (with-meta obj {:type t})))

;***************** logic ********************

  (defn or= 
    ([expr coll] (ap or= expr coll))
    ([expr o & ors]
    (eval `(or ~@(map (fn [o] `(= ~expr ~o)) (cons o ors))))))
  
  (defn and= 
    ([expr coll] (ap and= expr coll))
    ([expr o & ors]
    (eval `(and ~@(map (fn [o] `(= ~expr ~o)) (cons o ors))))))
  
  (defn and-not= 
    ([expr coll] (ap and-not= expr coll))
    ([expr o & ors]
    (eval `(and ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors))))))
  
  (defn or-not= 
    ([expr coll] (ap or-not= expr coll))
    ([expr o & ors]
    (eval `(or ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors))))))
  
  (defmacro or-> 
    "ex: (or-> 10 neg? (= 10))
    => true"
    [arg & exprs]
    `(or ~@(map (fn [expr] 
                  (if (symbol? expr) 
                    `(~expr ~arg)
                    (cons (first expr) (cons arg (next expr))))) 
                exprs)))
  
  (defmacro and-> 
    "ex: (and-> 10 pos? (= 10))
    => true"
    [arg & exprs]
    `(and ~@(map (fn [expr] 
                   (if (symbol? expr) 
                     `(~expr ~arg)
                     (cons (first expr) (cons arg (next expr))))) 
                 exprs)))

  (defmacro all-true? [d & preds]
    `(and ~@(map #(list % d) preds)))

  ;similar to all-true?
  (defn satisfies-all? 
    "return true if for each pred (pred v) is true"
    [v & preds]
    (every? identity ((a juxt preds) v)))

;*************** experiments ****************

  (defn repeater 
    "(repeater [[2 [3 [2 :foo] :bar] :woz] :bim [2 :yo]])
    => (:foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :foo 
        :foo :bar :foo :foo :bar :foo :foo :bar :woz :bim :yo :yo)"
    [coll]
    (mapcat (fn [[n & els]] 
              (if (count= els 1)
                (repeat n (first els)) 
                (a concat (repeat n (mapcat #(repeater [%]) els))))) 
            (map #(if (vector? %) % (vector 1 %)) coll)))
  
  (defn- nested-expr 
    [depth nested & wrap-expr]
    (if (= 0 depth)
      nested
      (eval `(~@wrap-expr ~(ap nested-expr (dec depth) nested wrap-expr)))))
  
  (defn map-in 
    "nested mapping 
    ex: (map-in inc [[1][2 3][3]]) 
    <=> (map-in 1 inc [[1][2 3][3]]) 
    <=> (map (partial map inc) [[1][2 3][3]])
    => ((2) (3 4) (4))"
    ([f coll] 
     (map (p map f) coll))
    ([depth f coll] 
     (a map [(nested-expr depth f partial map) coll])))

;************** namespaces ******************

  ; https://groups.google.com/forum/#!topic/clojure/GAGF38uI1-o
  ; by James reeves

  (defn- merge-meta! 
    "Destructively merge metadata from a source object into a target." 
    [source target] 
    (.setMeta target 
      (merge (meta source) 
             (select-keys (meta target) [:name :ns])))) 
  
  (defn immigrate 
    "Add all the public vars in a list of namespaces to the current 
    namespace." 
    [& namespaces] 
    (doseq [ns namespaces] 
      (require ns) 
      (doseq [[sym v] (ns-publics (find-ns ns))] 
        (merge-meta! v 
          (if (.isBound v) 
            (intern *ns* sym (var-get v)) 
            (intern *ns* sym)))))) 

  ;stuart sierra version

  ; (defn immigrate
  ;   "Create a public var in this namespace for each public var in the
  ;   namespaces named by ns-names. The created vars have the same name, root
  ;   binding, and metadata as the original except that their :ns metadata
  ;   value is this namespace."
  ;   [& ns-names]
  ;   (doseq [ns ns-names]
  ;     (require ns)
  ;     (doseq [[sym var] (ns-publics ns)]
  ;       (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
  ;         (if (.hasRoot var)
  ;           (intern *ns* sym (.getRoot var))
  ;           (intern *ns* sym))))))
