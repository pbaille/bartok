(ns utils.dom-part 
  (:use utils.utils)
  (:use [clocop core constraints])
  (:use utils.clocop))

(defn dom-part 
  "args:
  elements => seq of numbers
  size Integer
  sum Number
  returns all combinations of `size` elements that sums to `sum`"
  [elements size sum]
  (if (> size 1)
    (let [elems (apply sorted-set elements)
          results (map vector elems)
          allowed-elems 
            (fn [res] 
              (filter 
                #(let [rem-steps (- size (count res) 1)
                       rem-sum (- sum (apply + res) %)
                       res (conj res %)]
                  (and (-> res last (* rem-steps) (<= rem-sum))
                       (-> elems last (* rem-steps) (>= rem-sum))))
                (filter #(>= % (last res)) elems)))]
      
      (loop [results results]
        (if (or (-> results first count (= size)) (empty? results))
          results
          (recur (set (mapcat 
                        #(for [b (allowed-elems %)] (conj % b)) 
                        results ))))))
    (when (and (in? elements sum) (= size 1)) #{[sum]})))

; constraints solving implementation much slower...
(defn dom-part* [els siz sum]
  (let [[mi ma] ((juxt #(apply min %) #(apply max %)) els)
        var-syms (map #(-> % char str symbol) (range 97 (+ 97 siz)))
        var-str (map str var-syms)]
  (if (> siz 1)  
  (eval 
   `(with-store (store)
      (let ~(reduce (fn [acc [vsym vstr]]
                      (vec (concat acc [vsym `(int-var ~vstr ~mi ~ma)]))) 
                    [] (map list var-syms var-str))
        ~@(map (fn [i] `(constrain! ($mor $= ~i ~els))) var-syms)
        ~@(map (fn [[x1 x2]] `(constrain! ($<= ~x1 ~x2))) (partition 2 1 var-syms))
        (constrain! ($= ~sum ($+ ~@var-syms)))
        (map vals (solve! :solutions :all)))))
  (when (and (in? els sum) (= siz 1)) #{[sum]}))))

