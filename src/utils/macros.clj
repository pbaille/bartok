(ns utils.macros
  (:use utils.utils)
  (:require [clojure.walk :as w]))

(defmacro chefn [name check-fn args & tail]
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

(comment 
  (chefn funky-add 
     odd? 
    [x true 
     y false] 
    "comment" 
    (+ x y)))

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
  (let [docstring (when (string? (first body)) (first body))
        body (if docstring (next body) body)]
   `(do (defmulti ~name ~disp)
     ~@(map (fn [[v & fun-body]]
               (let [params (vec (take-nth 2 v))
                     types  (vec (take-nth 2 (next v)))
                     types (if (count= types 1) (first types) types)]
                 `(defmethod ~name ~types ~params ~@fun-body))) 
            body))))

(comment 
  (defmult multest b-types
    "yop comment"
    ([x :pitch
      y :pitch]
      ((b> distance) x y))
    ([x1 :mode
      x2 :pitch]
     ((b> vector) x1 x2))
    ([x clojure.lang.Keyword]
      (name x))))

;to define helper function that is traced by declare-helpers macro
(defmacro dehfn [name & body]
  `(defn- ~name ~@body))

(defmacro declare-helpers []
  `(declare ~@(map symbol 
                   (re-seq #"(?<=dehfn\s)[a-zA-Z+!\-_?0-9*~#@''`/.$=]*(?=\s)" 
                           (slurp (str "src/" *file*))))))

; (declare-helpers)
; (defn hello-user [name] (greet name))
; (dehfn greet [name] (str "Hello my dear " name))

(defmacro env-h [] 
  (let [ks (keys &env)]
    (zipmap (map keyword ks) (map symbol ks))))

(defmacro show-env 
  ([] `(show-env "env"))
  ([message] 
  (let [ks (keys &env)] 
    `(do
       (println (str "\n*** " ~message " ***"))
       (clojure.pprint/pprint ~(zipmap (map keyword ks) (map symbol ks)))
       (println "***\n"))))) 
        
;don't see the point of this...
(defmacro show-form [] (println (next &form)))

(defmacro dr-e 
  ([expr] 
   `(dr-e "auto-catch repl" ~expr))
  ([msg expr]
   `(try ~expr (catch Exception e# (do (pp ~msg)(dr))))))

;NOOOB !
; (defmacro p1-fn [name arg1 argv & body]
;   `(do 
;      (defn ~name ~argv ~@body)
;      (def ~name (p ~name ~arg1))))

; ;(macroexpand-1 '(p1-fn add 2 [aa b] (+ aa b)))
; ;=>(do (clojure.core/defn add [a b] (+ a b)) (def add (utils.utils/p add 2)))
; ;(p1-fn add 2 [a b] (+ a b))
; ;(add 1) => 3


;from mikera stackoverflow

(defmacro functionize [macro]
  `(fn [& args#] (eval (cons '~macro args#))))

(defmacro apply-macro [macro args]
   `(apply (functionize ~macro) ~args))

;***

;function def with default for arguments, single arity function only
(defmacro defnaults [name & tail]
  "function constructor with defaults args values
  
  ex1: with defaults val for all args 
  
    (defnaults adder [a 1 b 1 c 1] (+ a b c))
    (adder 2 2 2) => 6
    (adder 2 1) => 4
    (adder 1) => 3
    (adder) => 3
  
  ex1: with defaults val for the 2 last args only 
  
    (defnaults adder [a _ b 1 c 1] (+ a b c))
    (adder 2 2 2) => 6
    (adder 2 1) => 4
    (adder 1) => 3
    (adder) => ArityException Wrong number of args (0)
  "
  (let [docstring (when (string? (first tail))(first tail))
        argv (if docstring (second tail) (first tail))
        cnt (/ (count argv) 2)
        no-defaults-cnt (count (take-while #(= (second %) '_) (partition 2 argv)))
        args (vec (take-nth 2 argv))
        body (if docstring (nnext tail) (next tail))]
   `(defn ~name
      ; define inferiors arity 
      ~@(map (fn [x] (let [args (vec (take x args))]
               `(~args (~name ~@(fill-with args cnt :*))))) 
             (range no-defaults-cnt cnt))
      ;define main arity
      (~args (let [~(vec (drop no-defaults-cnt args)) 
                   (map (fn [[a# b#]] (if (= a# :*) b# a#)) 
                        (partition 2 2 ~(vec (drop (* 2 no-defaults-cnt) argv))))]
       ~@body)))))

(defmacro f> [& funs] `#(-> % ~@funs))
(defmacro f>> [& funs] `#(->> % ~@funs))

;;;;;;;;;;;;;;;;; asf> and asf>> (mix of ( -> or ->>) and as->) ;;;;;;;;;;;;;;;;

(defn- replace_ [x sym] 
  (if (sequential? x)
    (if (in? x '_) 
      (map (fn [xx] (if (= '_ xx) sym xx)) x)
      x)
    x))

(defmacro asf> [& funs] 
(let [sym (gensym "x")]  
  `#(as-> % ~sym
      ~@(map (fn [x] 
               (if (in? (flatten x) sym) 
                 x 
                 (list* (first x) sym (next x))))
             (w/postwalk (fn [x] (replace_ x sym)) funs)))))

;(asf> (+ 2 _ (* 3 _)) (- 3)) 
;;=> #(as-> % x# (+ 2 x# (* 3 x#)) (- x# 3))

(defmacro asf>> [& funs] 
(let [sym (gensym "x")]  
  `#(as-> % ~sym
      ~@(map (fn [x] 
               (if (in? (flatten x) sym) 
                 x 
                 (concat x [sym])))
             (w/postwalk (fn [x] (replace_ x sym)) funs)))))

;(asf> (+ 2 _ (* 3 _)) (- 3)) 
;;=> #(as-> % x# (+ 2 x# (* 3 x#)) (- 3 x#))

;buggy


; (defnaults2 bob 
;   "blabla"
;   [a 1 b 2] 
;   (+ a b))

(defmacro juxt-for [& seqs]
  (let [syms (repeatedly (count seqs) (p gensym "x"))
        bindings (vec (interleave syms seqs))]
    `(for ~bindings (list ~@syms))))

; (defmacro juxt-for2 [& body]
;   (let [[seqs clauses] (split-with sequential? body)
;         syms (for [i (count seqs)] (p gensym (str "x" i)))
;         bindings (vec (interleave syms seqs))
;         with-clauses (into bindings (map (fn [[kw clau]] `(~kw (a ~clau ~syms))) clauses))]
;     `(for ~with-clauses (list ~@syms))))

