> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain))))))
(-1 -3 0 2 0 2 1 -3 0 2)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain))))))
(1 2 -1 -3 2 0 2 0 2 1)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) -3))
(-3 -3 1 -3 -1 -3 0 2 1 -3)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) -3))
(-3 1 -3 -1 -3 0 2 -1 -3 0)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) -3))
(-3 1 -3 -1 -3 0 2 0 2 1)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) [-3 2]))
(-3 2 2 1 -3 1 -3 -1 -3 -1)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) [-3 2]))
(-3 2 2 1 -3 -1 -2 0 -2 -3)
utils.prob=> (take 10 (constr-laz (fn [chain [el _]] (not= (abs el) (abs (last chain)))) [-3 2]))
(-3 2 2 1 -3 -1 -2 -3 -1 -2)
utils.prob=> #'utils.prob/constraint-markov-gen
utils.prob=> utils.prob=> #'utils.prob/constr-laz
utils.prob=> utils.prob=> (take 10 (constr-laz (fn [chain el] (not= (abs el) (abs (last chain)))) [-3 2]))
(-3 2 -1 0 2 -3 -2 1 2 -1)
utils.prob=> CompilerException java.lang.RuntimeException: Unable to resolve symbol: start in this context, compiling:(/private/var/folders/3w/l39tm8z56cs2ypk389nv_7lh0000gn/T/form-init8742729818389061758.clj:1210:13) 
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> nil
utils.prob=> utils.prob=> nil
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> NullPointerException   clojure.lang.Numbers.ops (Numbers.java:961)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (:A-1 :B-1 :C0 :C0 :D0 :A-1 :G-1 :E-1 :E-1 :E-1)
utils.prob=> utils.prob=> (:C0 :C0 :E0 :D0 :B-1 :B-1 :D0 :E0 :B-1 :D0)
utils.prob=> utils.prob=> (:D0 :E0 :B-1 :A-1 :C0 :G-1 :F#-1 :F#-1 :F#-1 :A-1)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> (:C0 :E0 :F#0 :G0 :G0 :A0 :G0 :F#0 :D0 :C0)
utils.prob=> utils.prob=> (:C0 :E0 :F#0 :D0 :F#0 :F#0 :F#0 :G0 :E0 :G0)
utils.prob=> utils.prob=> (:D0 :B-1 :B-1 :B-1 :G-1 :G-1 :B-1 :A-1 :F#-1 :E-1)
utils.prob=> utils.prob=> (:C0 :C0 :A-1 :A-1 :E-1 :D-1 :D-1 :D-1 :F#-1 :D-1)
utils.prob=> utils.prob=> (:C0 :C0 :A-1 :A-1 :E-1 :D-1 :F#-1 :E-1 :C-1 :C-1)
utils.prob=> utils.prob=> (:B-1 :D0 :B-1 :C0 :D0 :A-1 :G-1 :E-1 :E-1 :G-1)
utils.prob=> utils.prob=> (:B-1 :D0 :F#0 :A0 :B0 :G0 :G0 :B0 :F#0 :E0)
utils.prob=> utils.prob=> (:E0 :E0 :G0 :A0 :F#0 :F#0 :A0 :G0 :E0 :D0)
utils.prob=> utils.prob=> (:E0 :B-1 :A-1 :A-1 :A-1 :B-1 :A-1 :G-1 :B-1 :G-1)
utils.prob=> utils.prob=> (:G-1 :F#-1 :F#-1 :G-1 :E-1 :G-1 :E-1 :G-1 :E-1 :F#-1)
utils.prob=> utils.prob=> (:G-1 :B-1 :A-1 :F#-1 :E-1 :G-1 :B-1 :C0 :A-1 :A-1)
utils.prob=> utils.prob=> (:C0 :D0 :B-1 :D0 :D0 :D0 :E0 :D0 :C0 :A-1)
utils.prob=> utils.prob=> (:C0 :D0 :B-1 :B-1 :B-1 :G-1 :G-1 :G-1 :B-1 :C0)
utils.prob=> utils.prob=> (:G-1 :F#-1 :A-1 :F#-1 :G-1 :A-1 :A-1 :B-1 :G-1 :G-1)
utils.prob=> utils.prob=> (:C0 :D0 :B-1 :B-1 :F#-1 :E-1 :C-1 :C-1 :E-1 :F#-1)
utils.prob=> utils.prob=> (:C0 :D0 :B-1 :B-1 :B-1 :C0 :A-1 :A-1 :A-1 :F#-1)
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> dr-1-1001 => dr-1-1001 => data 
{[2 -2] {-1 2}, [2 1] {1 2}, [-3 1] {1 2}, [-3 -2] {-1 2}, [2 -2 -1] {0 3}, [-3 -2 -1] {-2 3}, [-2] {-2 0.30000000000000004, -3 0.2, -1 0.4, 1 0.2, 2 0.1}, [1] {-3 0.1, -1 0.2, 1 0.30000000000000004, 2 0.1, -2 0.1, 0 0.1}, [1 0 -1] {0 3}, [-2 -1 0] {-3 3}, [-3 2 1] {1 3}, [-2 1 1] {0 3}, [-2 -2 1] {-3 3}, [0 0 1] {-2 3}, [1 -3 2] {-1 3}, [-2 2 -3] {2 3}, [-1 -2 2] {-3 3}, [1 0] {-1 2}, [1 -1] {-3 2, -2 2}, [-2 -1] {-2 4, 1 2, 0 2}, [-3 2] {-1 2, 1 2}, [2 -3] {2 2}, [-3 1 1] {2 3}, [2 1 1] {-1 3}, [-3] {-2 0.1, 2 0.2, 1 0.1, 0 0.2, -1 0.1}, [2] {-1 0.1, -2 0.1, -3 0.1, 1 0.1}, [-2 -1 -2] {-2 3, 2 3}, [-2 -1 1] {-1 3}, [1 -1 -2] {-2 3}, [2 -3 2] {1 3}, [-2 1 -3] {2 3}, [-2 -2 -3] {-2 3, -1 3}, [1 1 2] {-2 3}, [-1 0 -3] {0 3}, [-1 0] {-3 2, 1 2}, [0 0] {0 2, 1 2}, [0 -1] {0 2}, [-2 1] {-3 2, 1 2}, [-2 -2] {-3 4, 1 2}, [1 1] {2 2, -1 2, 0 2}, [1 -2] {-1 2}, [-3 0 0] {0 3}, [1 -1 -3] {1 3}, [-1 -2] {-2 4, -1 2, 2 2, 1 2}, [0 -2] {-2 2}, [-1 1] {-1 2}, [0 1] {-2 2}, [1 -3] {2 2}, [-2 -3] {-2 2, -1 2}, [1 2] {-2 2}, [-2 2] {-3 2}, [2 -1 -2] {-1 3}, [-3 -1 -2] {1 3}, :depth 3, [-1 -3] {1 2}, [0 -3] {0 4}, [-3 0 -3] {0 3}, [0 -3 0] {-3 3, 0 3}, [-2 -3 -1] {-2 3}, [-1 -2 -1] {1 3}, [-1 1 -1] {-3 3}, [-1 -3 1] {1 3}, [0] {-2 0.1, -3 0.2, 0 0.2, 1 0.2, -1 0.1}, [-1] {-2 0.5, 1 0.1, -3 0.1, 0 0.2}, [-3 2 -1] {-2 3}, [1 -2 -1] {-2 3}, [1 1 -1] {-2 3}, [1 1 0] {-1 3}, [0 0 0] {1 3}, [0 -1 0] {1 3}, [-2 -3 -2] {-1 3}, [1 2 -2] {-1 3}, [-1 -2 -2] {1 3, -3 3}, [0 -2 -2] {-3 3}, [0 1 -2] {-1 3}, [-1 -2 1] {1 3}, [2 -1] {-2 2}, [-3 0] {-3 2, 0 2}, [-3 -1] {-2 2}}
dr-1-1001 => (get data [1 1])
{2 2, -1 2, 0 2}
dr-1-1001 => dr-2-1002 => dr-2-1002 => #'utils.prob/c-markov-gen-with-acc
dr-2-1002 => dr-2-1002 => ()
(2 -2 -1 0 -3 0 -3 2 -1 0)
(:E0 :C0 :B-1 :B-1 :F#-1 :F#-1 :C-1 :E-1 :D-1 :D-1)
dr-1-1001 => ()
(:B-1 :G-1 :F#-1 :D-1 :E-1 :F#-1 :F#-1 :F#-1 :F#-1 :G-1)
utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> (clojure.stacktrace/e)
java.lang.IndexOutOfBoundsException: null
 at clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
    clojure.lang.PersistentVector.nth (PersistentVector.java:111)
    clojure.lang.APersistentVector.invoke (APersistentVector.java:264)
    utils.prob$wrand.invoke (prob.clj:70)
    utils.prob$eval37089$c_markov_gen_with_acc__37090$fun__37092.doInvoke (form-init8742729818389061758.clj:1798)
    clojure.lang.RestFn.invoke (RestFn.java:494)
    utils.prob$eval37089$c_markov_gen_with_acc__37090$fn__37101.invoke (form-init8742729818389061758.clj:1822)
    utils.prob$eval37185.invoke (form-init8742729818389061758.clj:2045)
nil
utils.prob=> (-1 -1 0 2 -2 1 -2 2 1 -1)
(:B-1 :A-1 :A-1 :C0 :A-1 :B-1 :G-1 :B-1 :C0 :B-1)
utils.prob=> utils.prob=> (-2 0 -3 2 1 2 2 0 2 1)
(:A-1 :A-1 :E-1 :G-1 :A-1 :C0 :E0 :E0 :G0 :A0)
utils.prob=> utils.prob=> (2 1 -3 2 -3 -3 0 1 -1 -2)
(:E0 :F#0 :C0 :E0 :B-1 :F#-1 :F#-1 :G-1 :F#-1 :D-1)
utils.prob=> utils.prob=> (2 -2 1 -2 2 1 -3 0 2 -2)
(:E0 :C0 :D0 :B-1 :D0 :E0 :B-1 :B-1 :D0 :B-1)
utils.prob=> utils.prob=> (-1 2 2 -2 -2 0 -3 2 -3 -1)
(:B-1 :D0 :F#0 :D0 :B-1 :B-1 :F#-1 :A-1 :E-1 :D-1)
utils.prob=> utils.prob=> (-1 -2 -1 -1 -1 0 -1 0 2 -2)
(:B-1 :G-1 :F#-1 :E-1 :D-1 :D-1 :C-1 :C-1 :E-1 :C-1)
utils.prob=> utils.prob=> (-2 2 1 -1 -2 -1 -1 2 -1 0)
(:A-1 :C0 :D0 :C0 :A-1 :G-1 :F#-1 :A-1 :G-1 :G-1)
utils.prob=> utils.prob=> (-3 2 -3 -3 0 1 -1 2 2 -2)
(:G-1 :B-1 :F#-1 :C-1 :C-1 :D-1 :C-1 :E-1 :G-1 :E-1)
utils.prob=> utils.prob=> (-3 2 1 -3 2 -3 -3 0 1 -1)
(:G-1 :B-1 :C0 :G-1 :B-1 :F#-1 :C-1 :C-1 :D-1 :C-1)
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
utils.prob=> utils.prob=> (2 -1 0 2 1 -2 -2 -1 -1 -1)
(:E0 :D0 :D0 :F#0 :G0 :E0 :C0 :B-1 :A-1 :G-1)
utils.prob=> utils.prob=> (-2 -1 2 1 -2 2 1 -3 2 1)
(:A-1 :G-1 :B-1 :C0 :A-1 :C0 :D0 :A-1 :C0 :D0)
utils.prob=> utils.prob=> (-2 -1 -1 2 2 0 2 1 -3 2)
(:A-1 :G-1 :F#-1 :A-1 :C0 :C0 :E0 :F#0 :C0 :E0)
utils.prob=> utils.prob=> (-2 -1 -1 0 2 -2 -1 -1 2 -1)
(:A-1 :G-1 :F#-1 :F#-1 :A-1 :F#-1 :E-1 :D-1 :F#-1 :E-1)
utils.prob=> utils.prob=> (2 -1 0 -1 -1 2 1 -3 2 -3)
(:E0 :D0 :D0 :C0 :B-1 :D0 :E0 :B-1 :D0 :A-1)
utils.prob=> utils.prob=> (-2 -2 2 1 -3 2 -3 0 1 -1)
(:A-1 :F#-1 :A-1 :B-1 :F#-1 :A-1 :E-1 :E-1 :F#-1 :E-1)
utils.prob=> utils.prob=> (-2 -2 0 -3 2 1 -1 -2 1 2)
(:A-1 :F#-1 :F#-1 :C-1 :E-1 :F#-1 :E-1 :C-1 :D-1 :F#-1)
utils.prob=> utils.prob=> (2 -1 0 2 -2 -2 0 -3 2 -3)
(:E0 :D0 :D0 :F#0 :D0 :B-1 :B-1 :F#-1 :A-1 :E-1)
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> (2 -1 0 -3 2 1 -3 2 -3 -3)
(:E0 :D0 :D0 :A-1 :C0 :D0 :A-1 :C0 :G-1 :D-1)
utils.prob=> utils.prob=> (2 -1 0 2 2 0 2 1 -3 -3)
(:E0 :D0 :D0 :F#0 :A0 :A0 :C1 :D1 :A0 :E0)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 -3 -2 -2 2 -1 0 1 -2)
(:D0 :C0 :G-1 :E-1 :C-1 :E-1 :D-1 :D-1 :E-1 :C-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 0 0 -1 2 -3 1)
(:D0 :C0 :G-1 :E-1 :E-1 :E-1 :D-1 :F#-1 :C-1 :D-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 -1 2 -2 2 -1 0)
(:D0 :C0 :G-1 :E-1 :D-1 :F#-1 :D-1 :F#-1 :E-1 :E-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 -1 2 2 -3 1 -2)
(:D0 :C0 :G-1 :E-1 :D-1 :F#-1 :A-1 :E-1 :F#-1 :D-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 0 1 -2 2 1 -1)
(:D0 :C0 :G-1 :E-1 :E-1 :F#-1 :D-1 :F#-1 :G-1 :F#-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 0 0 -1 2 2 2)
(:D0 :C0 :G-1 :E-1 :E-1 :E-1 :D-1 :F#-1 :A-1 :C0)
utils.prob=> utils.prob=> (1 -1 -3 -2 -2 1 -1 2 -2 2)
(:D0 :C0 :G-1 :E-1 :C-1 :D-1 :C-1 :E-1 :C-1 :E-1)
utils.prob=> utils.prob=> (1 -1 -3 -2 -1 2 -2 2 -3 1)
(:D0 :C0 :G-1 :E-1 :D-1 :F#-1 :D-1 :F#-1 :C-1 :D-1)
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
(1utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:107)
(1utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 0 0 -2 -2 1 1 -2 2)
(:D0 :C0 :C0 :C0 :A-1 :F#-1 :G-1 :A-1 :F#-1 :A-1)
utils.prob=> utils.prob=> (1 -1 0 -1 2 0 -2 -2 2 0)
(:D0 :C0 :C0 :B-1 :D0 :D0 :B-1 :G-1 :B-1 :B-1)
utils.prob=> utils.prob=> (1 -1 0 -1 -3 1 -1 2 -1 2)
(:D0 :C0 :C0 :B-1 :F#-1 :G-1 :F#-1 :A-1 :G-1 :B-1)
utils.prob=> utils.prob=> (1 -1 0 0 -2 -2 1 1 -2 2)
(:D0 :C0 :C0 :C0 :A-1 :F#-1 :G-1 :A-1 :F#-1 :A-1)
utils.prob=> utils.prob=> (1 -1 0 0 -2 -2 1 1 -2 0)
(:D0 :C0 :C0 :C0 :A-1 :F#-1 :G-1 :A-1 :F#-1 :F#-1)
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 -2 1 1 -3 2 -2 -1 -2)
(:D0 :C0 :A-1 :B-1 :C0 :G-1 :B-1 :G-1 :F#-1 :D-1)
utils.prob=> utils.prob=> (1 -1 2 -2 -1 1 1 2 -1 0)
(:D0 :C0 :E0 :C0 :B-1 :C0 :D0 :F#0 :E0 :E0)
utils.prob=> utils.prob=> (1 -1 -2 -3 -2 1 2 1 1 -3)
(:D0 :C0 :A-1 :E-1 :C-1 :D-1 :F#-1 :G-1 :A-1 :E-1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 2 2 -3 -1 2 2 -3 -1)
(:D0 :C0 :E0 :G0 :D0 :C0 :E0 :G0 :D0 :C0)
utils.prob=> utils.prob=> (1 -1 2 2 -3 -2 -1 1 -3 -3)
(:D0 :C0 :E0 :G0 :D0 :B-1 :A-1 :B-1 :F#-1 :C-1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 0 -2 0 1 0 1 1 0)
(:D0 :C0 :C0 :A-1 :A-1 :B-1 :B-1 :C0 :D0 :D0)
utils.prob=> utils.prob=> (1 -1 0 -2 0 2 0 -3 2 -1)
(:D0 :C0 :C0 :A-1 :A-1 :C0 :C0 :G-1 :B-1 :A-1)
utils.prob=> utils.prob=> (1 -1 -2 0 1 0 -2 -3 2 -1)
(:D0 :C0 :A-1 :A-1 :B-1 :B-1 :G-1 :D-1 :F#-1 :E-1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 1 2 1 -2 -1 1 -2 -1)
(:D0 :C0 :D0 :F#0 :G0 :E0 :D0 :E0 :C0 :B-1)
utils.prob=> utils.prob=> (1 -1 -3 0 0 2 -1 0 -1 0)
(:D0 :C0 :G-1 :G-1 :G-1 :B-1 :A-1 :A-1 :G-1 :G-1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 0 1 0 2 0 1 2 0)
(:D0 :C0 :C0 :D0 :D0 :F#0 :F#0 :G0 :B0 :B0)
utils.prob=> utils.prob=> (1 -1 0 1 0 2 0 1 2 2)
(:D0 :C0 :C0 :D0 :D0 :F#0 :F#0 :G0 :B0 :D1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> (1 -1 -3 -1 -3 2 -1 1 1 0)
(:D0 :C0 :G-1 :F#-1 :C-1 :E-1 :D-1 :E-1 :F#-1 :F#-1)
utils.prob=> utils.prob=> (1 -1 0 -3 0 -1 -1 0 1 1)
(:D0 :C0 :C0 :G-1 :G-1 :F#-1 :E-1 :E-1 :F#-1 :G-1)
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> ()
()
utils.prob=> utils.prob=> ()
()
utils.prob=> utils.prob=> ()
()
utils.prob=> utils.prob=> ()
utils.prob=> utils.prob=> ()
utils.prob=> utils.prob=> (when-let [a false] "yo")
nil
utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> ()
utils.prob=> utils.prob=> #'utils.prob/c-markov-gen-with-acc
utils.prob=> utils.prob=> #'utils.prob/c-laz
utils.prob=> utils.prob=> dr-1-1003 => dr-1-1003 => nil
dr-1-1003 => dr-1-1003 => ()
()
utils.prob=> (take 10 nil)
()
utils.prob=> 
ArityException Wrong number of args (0) passed to: utils/src  clojure.lang.Compiler.macroexpand1 (Compiler.java:6496)
utils.prob=> utils.prob=> :reloading (vendors.debug-repl utils.utils utils.all bartok.primitives bartok.midi.overtone-midi bartok.rythmn.rval bartok.types.note bartok.state bartok.structure bartok.composition.utils bartok.melody.all bartok.harmony.all bartok.rythmn.all bartok.composition.rythmic-step-pattern bartok.xml.parser bartok.xml.transform utils.macros utils.noise bartok.melody.melodic-domain bartok.melody.analysis bartok.melody.t-analysis bartok.t-primitives utils.interpolator bartok.melody.strategies utils.t-utils vendors.profile bartok.melody.step-pattern vendors.into-edn-ex bartok.harmony.h-function bartok.harmony.harmony bartok.xml.analysis bartok.midi.transform bartok.composition.all bartok.core bartok.rythmn.utils bartok.rythmn.random-line bartok.midi.parser utils.prob bartok.midi.midi z.contracts-try bartok.rythmn.humanize bartok.rythmn.weighted-line vendors.compression vendors.markov vendors.perlin-noise vendors.nrepl-middleware utils.dom-part)
:ok
utils.prob=> utils.prob=> #'bartok.core/mc1
utils.prob=> utils.prob=> CompilerException java.lang.RuntimeException: Unable to resolve symbol: m-note-line-from in this context, compiling:(/private/var/folders/3w/l39tm8z56cs2ypk389nv_7lh0000gn/T/form-init8742729818389061758.clj:3201:8) 
utils.prob=> utils.prob=> #'bartok.core/mc1
utils.prob=> utils.prob=> (ns bartok.core)
nil
bartok.core=> CompilerException java.lang.RuntimeException: Unable to resolve symbol: len in this context, compiling:(/private/var/folders/3w/l39tm8z56cs2ypk389nv_7lh0000gn/T/form-init8742729818389061758.clj:3210:8) 
bartok.core=> bartok.core=> (mc1 10)
({:velocity 60, :channel 1, :pitch {:name :Gb2, :val 90, :octave 2, :pitch-class {:name :Gb, :val 6, :natural {:name G, :val 4, :pitch-val 7}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 0N}} {:velocity 60, :channel 1, :pitch {:name :Eb2, :val 87, :octave 2, :pitch-class {:name :Eb, :val 3, :natural {:name E, :val 2, :pitch-val 4}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 1/4}} {:velocity 60, :channel 1, :pitch {:name :Gb2, :val 90, :octave 2, :pitch-class {:name :Gb, :val 6, :natural {:name G, :val 4, :pitch-val 7}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 1/2}} {:velocity 60, :channel 1, :pitch {:name :Eb2, :val 87, :octave 2, :pitch-class {:name :Eb, :val 3, :natural {:name E, :val 2, :pitch-val 4}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 3/4}} {:velocity 60, :channel 1, :pitch {:name :Db2, :val 85, :octave 2, :pitch-class {:name :Db, :val 1, :natural {:name D, :val 1, :pitch-val 2}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 1N}} {:velocity 60, :channel 1, :pitch {:name :Ab1, :val 80, :octave 1, :pitch-class {:name :Ab, :val 8, :natural {:name A, :val 5, :pitch-val 9}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 5/4}} {:velocity 60, :channel 1, :pitch {:name :Gb1, :val 78, :octave 1, :pitch-class {:name :Gb, :val 6, :natural {:name G, :val 4, :pitch-val 7}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 3/2}} {:velocity 60, :channel 1, :pitch {:name :Ab1, :val 80, :octave 1, :pitch-class {:name :Ab, :val 8, :natural {:name A, :val 5, :pitch-val 9}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 7/4}} {:velocity 60, :channel 1, :pitch {:name :Gb1, :val 78, :octave 1, :pitch-class {:name :Gb, :val 6, :natural {:name G, :val 4, :pitch-val 7}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 2N}} {:velocity 60, :channel 1, :pitch {:name :Gb1, :val 78, :octave 1, :pitch-class {:name :Gb, :val 6, :natural {:name G, :val 4, :pitch-val 7}, :alteration {:val -1, :name :b}}}, :duration 1/4, :position {:cycle 0, :bar 0, :sub 9/4}})
bartok.core=> #'bartok.core/mc
bartok.core=> bartok.core=> (mc)
(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
bartok.core=> *midi-out*
#<Atom@bee2ea6: {:receiver #<MidiOutReceiver com.sun.media.sound.MidiOutDevice$MidiOutReceiver@5e6d855c>, :name "Bus IAC 2", :description "Gestionnaire IAC Bus IAC 2", :vendor "Apple Inc.", :version "Unknown version", :sources 0, :sinks 2147483647, :info #<MidiOutDeviceInfo Bus IAC 2>, :device #<MidiOutDevice com.sun.media.sound.MidiOutDevice@109ec777>}>
bartok.core=> #'bartok.core/mc
bartok.core=> bartok.core=> (mc)
(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
bartok.core=> {:bars ({:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4}), :harmony (), :tempo 120}
bartok.core=> bartok.core=> #'bartok.core/mc1
bartok.core=> bartok.core=> #'bartok.core/mc2
bartok.core=> bartok.core=> {:bars ({:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4} {:name :4|4, :val 4, :numerator 4, :denominator 4}), :harmony (), :tempo 120}
bartok.core=> bartok.core=> #'bartok.core/mc
bartok.core=> bartok.core=> (mc)
(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
bartok.core=> 