(ns utils.utils)

(defn call [^String nm & args]
    (when-let [fun (ns-resolve *ns* (symbol nm))]
        (apply fun args)))

(defn keyword-cat [& args] 
  (-> (apply str (map name args)) keyword))

(defn between
  ([a b] (between a (first b) (second b)))
  ([a b1 b2] (and (>= a b1) (<= a b2))))

(defn count= [coll c] (-> coll count (= c)))

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

(defn abs [x] (if (neg? x) (* x -1) x))

(defn rand-int-between [a b] (rand-nth (range a b)))

(defn parse-int [s] (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn kw->str [kw] (-> kw str (subs 1)))

(defn kw-or-str? [x] (or (keyword? x) (string? x)))

(defn rotate [coll n]
  (let [n (if (>= n 0) n (+ n (count coll)))
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


(defn div-mod [x div] [(int (/ x div)) (rem x div)])

(defn steps 
  "return steps between each adjacent items of coll"
  [coll] 
  (reduce #(conj %1 (apply - (reverse %2))) [] (partition 2 1 coll)))

