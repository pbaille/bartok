(ns bartok.harmony.harmony
  (:use utils.utils)
  (:use bartok.harmony.h-function)
  (:use bartok.primitives))

(defn harmonic-context 
  ([current] (harmonic-context (mother-mode (b> current)) current))
  ([center current]
     (let [center (b> center)
           current (b> current)
           h-funct (apply c-interval (map #(-> % mother-root pitch) [center current]))]
       (with-type 
         'HarmonicContext
         {:center center
          :current current
          :h-function (h-function h-funct)}))))

(defn abs-move 
  ([h-context h-function]
    (conj h-context {:current (transpose (:center h-context) (c-interval (:degree h-function)))
                     :h-function h-function}))
  ([hc hf d-class]
    (intra-abs-move (abs-move hc hf) (if (number? d-class) d-class (-> (b> d-class) :val inc)))))

(defn rel-move 
  ([h-context h-function]
    (conj h-context {:current (transpose (:current h-context) (c-interval (:degree h-function)))
                     :h-function h-function}))
  ([hc hf d-class]
    (intra-abs-move (rel-move hc hf) (if (number? d-class) d-class (-> (b> d-class) :val inc)))))

(defmethod intra-abs-move ['HarmonicContext :number][hc n]
  (conj hc {:current (-> (:current hc) (intra-abs-move n))}))

(defmethod intra-rel-move ['HarmonicContext :number][hc n]
  (conj hc {:current (-> (:current hc) (intra-rel-move n))}))

(defmethod relative ['HarmonicContext :mode-class] [hc mc]
  (conj hc {:current (-> (:current hc) (relative mc))}))

(defn centerize [h-context]
  {:pre [(type= h-context 'HarmonicContext)]}
  (conj h-context 
    {:center (-> h-context :current mother-mode) 
     :h-function (h-function :T)}))

(defn set-center [h-context new-center]
  {:pre [(type= h-context 'HarmonicContext)]}
  (let [i  (c-interval (mother-root (b> new-center)) 
                     (mother-root (:current h-context)))
        d (if (< (:val i) 0) (relative (c-interval-class i)) (c-interval-class i))]
    (conj h-context 
      {:center (-> h-context :current mother-mode) 
       :h-function (h-function d)})))
  