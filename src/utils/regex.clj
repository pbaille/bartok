(ns utils.regex)

(defn escape-regex-reserved [str] 
  (-> str
    (clojure.string/replace #"\+" "\\\\+")
    (clojure.string/replace #"\." "\\\\.")
    (clojure.string/replace #"\?" "\\\\?")
    (clojure.string/replace #"\|" "\\\\|")
    (clojure.string/replace #"\$" "\\\\$")
    (clojure.string/replace #"\^" "\\\\^")))

(defn re-wrap 
  "wrap a regex ex: (re-wrap #\"a|b|c\") => #\"(a|b|c)\""
  [re] 
  (re-pattern (str "(" (str re) ")")))

(defn re-unwrap 
  "unwrap a regex if it's wrapped 
  ex: (re-wrap #\"(a|b|c)\") => #\"a|b|c\""
  [re] 
  (let [s (str re)] 
    (if (and (= \( (first s))(= \) (last s))) 
      (re-pattern (apply str (next (butlast s))))
      re)))

(defn re-wrapv 
  "wrap a regex ex: (re-wrap #\"a|b|c\") => #\"[a|b|c]\""
  [re] 
  (re-pattern (str "[" (str re) "]")))

(defn re-unwrapv 
  "unwrap a regex if it's wrapped 
  ex: (re-wrap #\"[a|b|c]\") => #\"a|b|c\""
  [re] 
  (let [s (str re)] 
    (if (and (= \[ (first s))(= \] (last s))) 
      (re-pattern (apply str (next (butlast s))))
      re)))

; (apply str (next (butlast "(123)")))

(defn re-? 
  "add ? at the end of a regex"
  [re] (re-pattern (str (str re) "?")))

(defn re-or 
  "ex: (re-or #'(a|b)' #'(c|d)') => #'(a|b)|(c|d)' "
  [& pats]
  (re-pattern (apply str (interpose "|" (map str pats)))))

(defn coll->or-pat 
  "ex : (coll->or-pat [:a :b]) => #'a|b' "
  [coll]
  (->> coll
    (map (comp escape-regex-reserved name))
    (sort-by count)
    reverse
    (interpose "|")
    (apply str)
    re-pattern))