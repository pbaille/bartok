(ns utils.domain-partition)

(defn dom-part [elements size sum]
  
  (let [elems (apply sorted-set elements)
        results (map vector elements)
        allowed-elems 
          (fn [res] 
            (filter 
              #(let [rem-steps (- size (count res) 1)
                     rem-sum (- sum (apply + res) %)
                     res (conj res %)]
                (and (-> res last (* rem-steps) (<= rem-sum))
                     (-> elements last (* rem-steps) (>= rem-sum))))
              (filter #(>= % (last res)) elements)))]
    
    (loop [results results]
      (if (or (-> results first count (= size)) (empty? results))
        results
        (recur (set (mapcat 
                      #(for [b (allowed-elems %)] (conj % b)) 
                      results )))))))




