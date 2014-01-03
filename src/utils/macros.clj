(ns utils.macros
  (:use [utils.utils]))

(defmacro defn-check [name check-fn args & tail]
  (let [docstring (if (string? (first tail)) (first tail))
        tail (if docstring (next tail) tail)
        params (take-nth 2 args)
        checks (take-nth 2 (next args))
        cf `check-fn#]
    `(let [~cf ~check-fn]
       (defn ~name
         ~@(if docstring [docstring])
          ~(vec params)
         {:pre ~(mapv (fn [p c]
                        `(= (~cf ~p) ~c))
                      params
                      checks)}
         ~@tail))))

(defmacro let-if [pred & body]
  `(if ~pred (let ~@body)))

;doesn't work
(defmacro def- [name val] `(def ^:private ~name ~val))

(defmacro public-first [& body]
  (let [body (sort-by
               #(cond 
                 (= (first %) 'defn-) 0
                 (= (first %) 'def-)  0
                 (= (first %) 'defn)  1
                 (= (first %) 'def)   1 ) 
               body)]
    `(do ~@body)))

; (public-first
;   (defn m [a] (greet a))
;   (defn- greet [a] (str "hello" a)))
