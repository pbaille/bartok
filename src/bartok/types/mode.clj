(in-ns 'bartok.types)

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

;************ construct **************

(defmulti mode b-types)

(defmethod mode :mode [n]
  (let [[r mc] (dash-split n)
         r (pitch-class (keyword r))
         mc (mode-class (keyword mc))
         pcs (pitch-classes-calc r (:degrees mc))]
    (build-mode n r mc pcs)))

(defmethod mode [:mode :number] [mn d]
  (let [mc (mode-class (keyword (second (dash-split mn))) d)
        _ (p (:name mc))
        r (nth (:pitch-classes (mode mn)) (dec d))
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

(defn relative [m new-mode-class]
  (let [nmc (mode-class new-mode-class)
        degree (-> nmc :degree :degree-class :val inc)
        mother-mode-name (keyword-cat (:name (mother-root m)) "-" (:mother nmc))
        _ (do (p mother-mode-name) (p degree))]
    (mode mother-mode-name degree)))

(defn intra-abs-move [m n]
  (mode (:name(mother-mode m)) n))

(defn intra-rel-move [m n]
  (mode (:name(mother-mode m)) 
        (#(inc (mod % 7)) (+ 7 (-> (degree-class-val m) dec) n))))

(defmethod transpose ['Mode 'Interval] [this interval]
  (let [r (transpose (:root this) interval)
        n (keyword-cat (:name r) "-" (:name (:mode-class this)))
        ps (map #(transpose % interval) (:pitch-classes this))]
    (build-mode n r mode-class ps)))




