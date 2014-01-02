(ns utils.utils
  (:require clojure.pprint))

(defn p [x] (clojure.pprint/pprint x))

(defn type= [obj type-sym]
  (= (type obj) type-sym))

(defn map-reduce [f init coll] 
  ((comp vec next) 
     (reduce (fn [acc el] 
               (conj acc (f (last acc) el))) 
              [init] 
              coll)))

; takes function that takes coll as third argument
(defn red-with-coll [f init coll] 
  (reduce #(f %1 %2 coll) init coll))

(defn named? [x]
  (or (keyword? x) (string? x) (symbol? x)))

(defn dash-split [x] 
  (when (named? x) (clojure.string/split (name x) #"\-")))

(defn types 
  ([arg] (type arg))
  ([arg & more] (vec (map type (concat [arg] more)))))

(defn with-type [t obj]
  {:pre [(named? t)]}
  (with-meta obj {:type t}))

(defn call [^String nm & args]
    (when-let [fun (ns-resolve *ns* (symbol nm))]
        (apply fun args)))

(defn keyword-cat [& args] 
  (-> (apply str (map name args)) keyword))

(defn between
  ([a b] (between a (first b) (second b)))
  ([a b1 b2] (and (>= a b1) (<= a b2))))

(defn median [& args]
  (/ (apply + args) (count args)))

(defn count= [coll c] (-> coll count (= c)))

(def not-nil? (complement nil?))

(defn mod12 [x] (mod x 12))
(defn mod12map [coll] (map mod12 coll))

(defn select-first [pred coll]
  (first (filter pred coll)))

(defn submap? [sub m] 
  (clojure.set/subset? (set sub) (set m)))

(defn first-where [sub-map coll]
  (select-first #(submap? sub-map %) coll))

(defn select-where [sub-map coll]
  (filter #(submap? sub-map %) coll))

(defn best [f coll]
  (reduce #(if (f %1 %2) %1 %2) coll))

(defn abs [x] (if (neg? x) (* x -1) x))

(defn rand-int-between [a b] (rand-nth (range a b)))

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn kw->str [kw] (-> kw str (subs 1)))

(defn kw-or-str? [x] (or (keyword? x) (string? x)))

(defn rotate [coll n]
  (let [c (count coll)
        n (if (>= n 0) (mod n c) (+ n (count coll)))
        splited (split-at n coll)]
    (concat (splited 1) (splited 0))))

(defn prime-factors [n]
  (loop [n n divisor 2 factors []]
    (if (< n 2)
      factors
      (if (= 0 (rem n divisor))
        (recur (/ n divisor) divisor (conj factors divisor))
        (recur n (inc divisor) factors)))))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(defn index-of [item coll] (first (pos #{item} coll)))

(defn div-mod [x div] [(int (/ x div)) (mod x div)])

(defn int-div [x div] (int (/ x div)))

(defn steps 
  "return steps between each adjacent items of coll"
  [coll] 
  (reduce #(conj %1 (apply - (reverse %2))) [] (partition 2 1 coll)))

; find the first value for kw key in map or nested maps 
(defn in> [m kw ]
  (when (map? m)
    (if-let [v (kw m)] 
      v (in> (apply merge (filter map? (vals m))) kw))))

