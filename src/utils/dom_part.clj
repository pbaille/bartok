(ns utils.dom-part 
  (:use utils.utils))

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