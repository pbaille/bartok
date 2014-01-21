(in-ns 'bartok.litterals.all)

(load "types/degree")
(load "types/interval")
(load "types/pitch_class")
(load "types/pitch")
(load "types/mode_class")

;********* helpers *********

(defn- pitch-classes-calc [root degrees]
  (cons root (map #(transpose root (interval %)) (next degrees))))

(defn build-mode [n r mc pcs]
  (with-type 'Mode {:name n :root r :mode-class mc :pitch-classes pcs}))

(def mothers 
  (for [mc [:Lyd :Lyd+ :Lyd#2] 
        r [:F :Bb :Eb :Ab :Db :Gb :Cb :Fb
              :C  :G  :D  :A  :E  :B  :F#]]
    (mode (kwcat r "-" mc))))

;************ construct **************

(b-construct mode
[:mode n]
  (let [[r mc] (dash-split n)
         r (pitch-class (keyword r))
         mc (mode-class (keyword mc))
         pcs (pitch-classes-calc r (:degrees mc))]
    (build-mode n r mc pcs))

['Mode m :number d]
  (let [mc (mode-class (keyword (second (dash-split (:name m)))) d)
        r (nth (:pitch-classes (mode (:name m))) (dec d))
        n (keyword-cat (:name r) "-" (:name mc))
        pcs (pitch-classes-calc r (:degrees mc))]
    (build-mode n r mc pcs)))

;************* methods *************
(defn- degree-class-val [this]
  (-> this :mode-class :degree :degree-class :val))

(defn mother-root [m]
  (let [d (degree-class-val m)]
    (first (rotate (:pitch-classes m) (* -1 d)))))

(defn mother-mode [m]
  (mode (keyword-cat (:name (mother-root m)) "-" (get-in m [:mode-class :mother]))))

(defmethod relative ['Mode :mode-class] [m new-mode-class]
  (let [nmc (mode-class new-mode-class)
        degree (-> nmc :degree :degree-class :val inc)
        mother-mode-name (keyword-cat (:name (mother-root m)) "-" (:mother nmc))]
    (mode mother-mode-name degree)))

(defmethod intra-abs-move ['Mode :number] [m n]
  (mode (:name (mother-mode m)) n))

(defmethod intra-rel-move ['Mode :number] [m n]
  (mode (:name (mother-mode m)) 
        (-> (degree-class-val m) (+ n) (mod 7) inc)))

(defmethod transpose ['Mode 'Interval] [this interval]
  (let [r (transpose (:root this) interval)
        n (keyword-cat (:name r) "-" (:name (:mode-class this)))
        ps (map #(transpose % interval) (:pitch-classes this))]
    (build-mode n r (:mode-class this) ps)))




