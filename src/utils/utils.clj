(ns utils.utils
  (:use midje.sweet)
  (:require clojure.pprint)
  (:require vendors.debug-repl)
  (:require [clojure.inspector :refer [inspect-tree]])
  (:require [clojure.contrib.math :as math]))

(declare p a ap c)
;***************** utils ********************

  (defn pp [& xs] (dorun (map clojure.pprint/pprint xs)))
  (defn pev [x] (do (clojure.pprint/pprint x) x))
  (def not-nil? (complement nil?))
  
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
  
  
  (defmacro or= 
    ([expr coll] `(or= ~expr ~@coll))
    ([expr o & ors]
    `(or ~@(map (fn [o] `(= ~expr ~o)) (cons o ors)))))
  
  (defmacro and= 
    ([expr coll] `(and= ~expr ~@coll))
    ([expr o & ors]
    `(and ~@(map (fn [o] `(= ~expr ~o)) (cons o ors)))))
  
  (defmacro and-not= 
    ([expr coll] `(and-not= ~expr ~@coll))
    ([expr o & ors]
    `(and ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors)))))
  
  (defmacro or-not= 
    ([expr coll] `(or-not= ~expr ~@coll))
    ([expr o & ors]
    `(or ~@(map (fn [o] `(not= ~expr ~o)) (cons o ors)))))
  
  (defmacro or-> [arg & exprs]
    `(or ~@(map (fn [expr] (if (symbol? expr) 
                             `(~expr ~arg)
                             (cons (first expr) (cons arg (next expr))))) 
                exprs)))
  
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
  (defn median [& args] (/ (apply + args) (count args)))
  (defn same-sign? [x y] (pos? (* x y)))
  
  (defn opposite-sign? [x y] 
    (or (and (pos? x) (neg? y)) 
        (and (neg? x) (pos? y))))
  
  (defn round [s n] 
    (.setScale (bigdec n) s java.math.RoundingMode/HALF_EVEN)) 
  
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
  
    ; max inclusive
  (defn range-by 
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
      (concat coll tail)))
  
  (defn seq1 
    "force 1 by one evaluation  of lazy sequence s"
    [s]
    (lazy-seq
      (when-let [[x] (seq s)]
        (cons x (seq1 (rest s))))))
  
  (defn partition-if
  "split coll where (pred elem next-elem) is true"
  [pred coll]
  (lazy-seq
   (if (second coll)
     (let [prv (pred (first coll)(second coll))
           temp (take-while 
                  #(not (pred (first %) (second %))) 
                  (partition 2 1 coll))
           lst (last (last temp))
           run (concat (mapv first temp) [lst])]
       (cons run (partition-if pred (seq (drop (count run) coll))))))))


;***************** maps *********************
  
  (defn map->sorted 
    "given a hash-map return the corresponding sorted-map"
    [m] (a sorted-map (a concat m)))sort
  
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
  
  (defn tups->h-map 
    "(tups->h-map [[:a 1][:b 2]]) => {:a 1 :b 2}"
    [tups-coll]
    (a hash-map (a concat tups-coll)))
  
  (defn dissoc-in [m key-vec]
    (let [firsts (vec (butlast key-vec))
          node (dissoc (get-in m firsts) (last key-vec))]
      (assoc-in m firsts node)))
  
  (defn submap? [sub m] 
    (clojure.set/subset? (set sub) (set m)))
  
  ; find the first value for kw key in map or nested maps 
  (defn in> [m kw ]
    (when (map? m)
      (if-let [v (kw m)] 
        v (in> (apply merge (filter map? (vals m))) kw))))
  
  (defn map-vals [f m]
    (a merge (map (fn [[k v]] {k (f v)}) m)))
  
  (defn map-keys [f m]
    (a merge (map (fn [[k v]] {(f k) v}) m)))
  
  (defn map-h  [f m]
    {:pre [(map? m)]}
    (let [hms (map (fn [[k v]] (f k v)) m)]
      (if (second hms) (a conj hms) (first hms))))
  
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

  ;(map-h (fn [k v] {k (inc v)}) {:a 1 :b 2})
  ;=> {:a 2 :b 3}
  
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
  
  ; (defn map-reduce-old [f init coll] 
  ;   ((c vec next) 
  ;      (reduce (fn [acc el] 
  ;                (conj acc (f (last acc) el))) 
  ;               [init] 
  ;               coll)))
  
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
  
  (defn remnil-map [f coll]
    "map coll with f and filter nils"
    (remove nil? (map f coll)))
  ;(remnil-map #(when (< 0 %) %) [-1 2 3])
  ;=> (2 3)
  
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
    "take the first truthy element of (map f coll)"
    (select-first (complement nil?) (map f coll)))
  
  ;(first-truthy #(when (< 0 %) %) [-1 2 3])
  ;=> 2
  
  (defn set-map [f coll]
    "call set on the result of map"
    (set (map f coll)))
  
  
  
  ; (defn first-where [sub-map coll]
  ;   (select-first #(submap? sub-map %) coll))
  
  ; (defn select-where [sub-map coll]
  ;   (filter #(submap? sub-map %) coll))
  
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

;*************** experiments ****************

  (defn repeater [coll]
    (mapcat (fn [[n & els]] 
              (if (count= els 1)
                (repeat n (first els)) 
                (a concat (repeat n (mapcat #(repeater [%]) els))))) 
            (map #(if (vector? %) % (vector 1 %)) coll)))
  
  ;(repeater [[2 [3 [2 :foo] :bar] :woz] :bim [2 :yo]])
  ;=> (:foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :foo :foo :bar :foo :foo :bar :foo :foo :bar :woz :bim :yo :yo)
  
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
