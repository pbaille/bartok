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


(defmacro def- [name value]
  `(def ~(vary-meta name assoc :private true) ~value))

; evil don't use
; move all forms with first sym that ends with "-" char to top of the file
; you can then keep your helpers at bottom of file
(defmacro public-first [& body]
  (let [body (sort-by 
               #(if (-> % first str last (= \-)) 0 1) 
               body)]
    `(do ~@body)))

;(public-first 
; (def aze 12) 
; (defn azeaze [] "azeaze") 
; (defn- qsd [] "qsd") 
; (def- az 456))

; => (do (defn- qsd [] "qsd") 
       ; (def- az 456) 
       ; (def aze 12) 
       ; (defn azeaze [] "azeaze"))


(defmacro defmult [name disp & body]
  `(do (defmulti ~name ~disp)
     ~@(map (fn [[v & fun-body]]
               (let [params (vec (take-nth 2 v))
                     types  (vec (take-nth 2 (next v)))
                     types (if (count= types 1) (first types) types)]
                 `(defmethod ~name ~types ~params ~@fun-body))) 
            body)))

; (defmult multest b-types
;   ([x :pitch
;     y :pitch]
;     ((b> distance) x y))
;   ([x1 :mode
;     x2 :pitch]
;    ((b> vector) x1 x2))
;   ([x clojure.lang.Keyword]
;     (name x)))

(defmacro dehfn [name & body]
  `(defn- ~name ~@body))

(defmacro declare-helpers []
  `(declare ~@(map symbol 
                   (re-seq #"(?<=dehfn\s)[a-zA-Z+!\-_?0-9*~#@''`/.$=]*(?=\s)" 
                           (slurp (str "src/" *file*))))))

; (declare-helpers)
; (defn hello-user [name] (greet name))
; (dehfn greet [name] (str "Hello my dear " name))


