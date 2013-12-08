(ns rand-music.abs-mode
  (:use [rand-music.constants])
  (:use [rand-music.pitch-class])
  (:use [rand-music.pitch])
  (:use [rand-music.m-degree])
  (:use [utils.utils]))

(defn mode? [m] (= (set (keys m)) #{:name :mother :degree :degrees :prio}))


(def modes
  (set (map
    (fn [[ch-name moth-name]]
      (let [moth-hash (mother-modes moth-name)
            degree (->> moth-hash :childs (pos #{ch-name}) first inc)
            prio   (-> moth-hash :modes_prio (nth (dec degree)))
            degrees (vec (sort #(< (m-degree-dist %1) (m-degree-dist %2)) prio))]
        {:name ch-name :mother moth-name :degree degree :prio prio :degrees degrees}))
    mother)))

(defn abs-mode-name? [k] (k mother))

(defn split-mode-name [mn]
  (let [[r m] (map keyword (clojure.string/split (name mn) #"\-"))]
    (if (and (abs-mode-name? m) (pitch-class?? r)) [r m] nil)))

(defn mode-name? [k]
  (if (split-mode-name k) true false))

(defn abs-mode [& args]
  (let [[a b c d] args]
    (cond
      (abs-mode-name? a)
        (select-first #(= a (:name %)) modes)
      (map? a)
        (select-first #(submap? a %) modes))))

(defmulti mode
  (fn [& args] (let [[a b c d] args]
     (cond
      (mode-name? a) :mode-name))))

(defmethod mode :mode-name [n]
  (apply #(into (abs-mode %2)
                (hash-map :root %1))
    (split-mode-name n)))

(mode :C#-Lyd)





