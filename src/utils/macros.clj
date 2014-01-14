(ns utils.macros
  (:use [utils.utils]))

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

;(chefn funky-add 
;  odd? 
; [x true 
;  y false] 
; "comment" 
; (+ x y))

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

(comment 
  (defmult multest b-types
    ([x :pitch
      y :pitch]
      ((b> distance) x y))
    ([x1 :mode
      x2 :pitch]
     ((b> vector) x1 x2))
    ([x clojure.lang.Keyword]
      (name x))))

(defn aze [a :aze] )

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
  (let [docstring (when (string? (first tail))(first tail))
        argv (if docstring (second tail) (first tail))
        args (vec (take-nth 2 argv))
        body (if docstring (nnext tail) (next tail))]
   `(defn ~name 
      ~@(when docstring [docstring])
      ~@(let [cnt (/ (count argv) 2)]
          (map (fn [x] (let [args (vec (take x args))]
                 `(~args (do ()(~name ~@(fill-with args cnt :default)))))) 
               (range cnt)))
      (~args
      (let [~args (map (fn [[a# b#]] (if (= a# :*) b# a#)) 
                       (partition 2 2 ~argv))]
        ~@body)))))

(comment
  (defnaults aze [a 1 b 2] (+ a b))
  (aze 2 2)  ;=> 4
  (aze 2)    ;=> 4
  (aze)      ;=> 3
  (aze :* 2) ;=> 3
)

(defmacro defnaults-old [name argv & body]
  `(defn ~name 
     ~@(let [cnt (/ (count argv) 2)]
         (map #(let [args (vec (take % (take-nth 2 argv)))]
                `(~args (a ~name ~(vec (fill-with args cnt :_))))) 
              (range cnt)))
     (~(vec (take-nth 2 argv)) 
     (let [~(vec (take-nth 2 argv)) (map (fn [[a# b#]](if (= a# :_) b# a#)) 
                                         (partition 2 2 ~argv) )]
       ~@body))))

(comment
  (defnaults aze [a 1 b 2] (+ a b))
  (aze 2 2)  ;=> 4
  (aze 2)    ;=> 4
  (aze)      ;=> 3
  (aze :_ 2) ;=> 3
)



;buggy


; (defnaults2 bob 
;   "blabla"
;   [a 1 b 2] 
;   (+ a b))

