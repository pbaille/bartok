(ns bartok.structure
  (:use utils.all)
  (:use bartok.state)
  (:use bartok.rythmn.rval)
  (:use bartok.primitives))

;-----------------------------------------------------
;;;;;;;;;;;;;;;;;;;;; grid ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-----------------------------------------------------
  
  ;;; helpers ;;;
  ;--------------
  
    (declare position pos-val before? after? pos-between?)

    (def ^:private default-grid
      (with-type 
        'Grid
        {:bars [[4 :4|4]]
         :tempo [[0 4 60]]}))

    (defn- expand-harmonies [g]
      (let [h (:harmony g)
            mh (if (map? h) h (a hash-map h))
            final (map (fn [[k v]] 
                         (hash-map :position k :mode (mode v))) 
                       mh)]
        (assoc g :harmony (sort-by :position final))))

    (defn- expand-bars [g]
      (assoc g :bars (map time-signature 
                          (repeater (:bars g)))))

  ;;; ** state ** ;;;
  ;------------------

      (defn grid 
        ([] (grid {}))
        ([m] (let [g ((c expand-harmonies expand-bars) 
                      (conj default-grid m))]
               (reset! *g* g))))

      (defn grid-assoc [& args] (swap! *g* #(ap assoc % args)))
  
  ;;; getters ;;;
  ;--------------

    (defn harmony-at [pos]
      (last (filter #(<= (pos-val (assoc pos :bar (-> % :position first) :sub (-> % :position second))) 
                       (pos-val pos)) 
                    (:harmony @*g*))))

    (defn mode-at [pos]
      (:mode (harmony-at pos)))

    (defn cycle-val [] (reduce + (map :val (:bars @*g*))))

    (defn modes-between [start-pos end-pos]
      (let [g @*g*
            [hs he] (map harmony-at [start-pos end-pos])
            cycle-complete? (<= (cycle-val) (apply - (map pos-val [end-pos start-pos])))]
        (if cycle-complete?
          (map :mode (:harmony g))
          (let [phsv ((juxt :bar :sub) start-pos) 
                phev ((juxt :bar :sub) end-pos)
                modes (set-map :mode [hs he])]
            (cond 
              (v-lt phsv phev) 
                (reduce #(conj %1 (:mode %2)) 
                  modes 
                  (filter (fn [{p :position}] (and (v-gt p phsv) (v-lt p phev))) 
                          (:harmony g))) 
              (v-gt phsv phev) 
                (reduce #(conj %1 (:mode %2)) 
                  modes 
                  (filter (fn [{p :position}] (or (v-lt p phev) (v-gt p phsv))) 
                          (:harmony g))))))))

  ;;; tempo ;;;
  ;------------
  
    (defn tempo-interpolator 
      ([] (cyclic-interpolator (-> @*g* :tempo) (cycle-val)))
      ([k] 
       (case k
         :linear (linear-interpolator (:tempo @*g*))
         :step   (step-interpolator   (:tempo @*g*)))))

    (defn tempo-at [x]
      (if (number? x)
        ((tempo-interpolator) x)
        ((tempo-interpolator)(pos-val x))))

;-----------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;; position ;;;;;;;;;;;;;;;;;;;;;;
;-----------------------------------------------------

  (defn g-pos 
    ([] (g-pos 0 0 0))
    ([{:keys [cycle bar sub] :or {cycle 0 bar 0 sub 0}}] 
     (g-pos cycle bar sub))
    ([cycl bar sub]
      (with-type 'Position {:cycle cycl :bar bar :sub sub})))

  ;;; grid-related ;;;
  ;-------------------

    (defn- bar-inc [p] 
      (let [g @*g*
            pos (conj p {:bar (-> p :bar inc)})]
        (if (< (:bar pos) (count (:bars g)))
          pos
          (conj pos {:cycle (-> pos :cycle inc) :bar 0}))))

    (defn- bar-dec [p] 
      (let [g @*g*
            pos (conj p {:bar (-> p :bar dec)})]
        (if (>= (:bar pos) 0)
          pos
          (conj pos {:cycle (-> pos :cycle dec) :bar (-> g :bars count dec)}))))

    (defn current-bar [pos]
      (-> @*g* :bars (nth (-> pos :bar))))

    (defn previous-bar [p]
      (let [g @*g*]
        (-> g :bars (nth (-> p :bar dec (mod (count (:bars g))))))))

    (defn previous-bars-val [p]
      (reduce + (take (:bar p) 
                      (map :val (@*g* :bars)))))

  ;;; generic ;;;
  ;--------------

    (b-meth to-num 'Position [p] (pos-val p))

    (defn pos->vec [{:keys [cycle bar sub]}]
      [cycle bar sub])

    (defn set-position [pos cycle bar sub]
      (assoc pos :cycle cycle :bar bar :sub sub))

    (defn set-sub [p s]
      (conj p {:sub s}))

    (defn pos+ [p rval]
      (let [sub (+ rval (:sub p))
            current-bar-val (:val (current-bar p))]
        (cond 
          (>= sub current-bar-val)
            (pos+ 
              (bar-inc (set-sub p 0)) 
              (- sub current-bar-val))
          (neg? sub)
            (pos+ 
              (bar-dec (set-sub p (-> p previous-bar :val))) 
              (+ sub (-> p :sub)))
          :else  
            (set-sub p sub))))

    (defn pos- [p rval] (pos+ p (- rval)))

    (defn num->pos [n]
      (pos+ (g-pos 0 0 0) n))

    (defn pos-val [p]
      (let [{:keys [cycle bar sub]} p]
        (+ (* (cycle-val) cycle) (previous-bars-val p) sub)))

    (defn before? [pos1 pos2]
      (pos? (apply - (map pos-val [pos2 pos1]))))

    (defn after? [pos1 pos2]
      (neg? (apply - (map pos-val [pos2 pos1]))))

    (defn eq? [pos1 pos2]
      (= 0 (apply - (map pos-val [pos2 pos1]))))

    (defn pos-between? 
      ([pos [pos1 pos2]] (pos-between? pos pos1 pos2))
      ([pos pos1 pos2]
       (let [[pos1 pos2] (if (before? pos1 pos2) [pos1 pos2] [pos2 pos1])]
         (or (and (after? pos pos1) (before? pos pos2)) 
             (or (eq? pos pos1) (eq? pos pos2))))))

  ;;; to-ms ;;;
  ;------------

    (defn note-to-ms 
      [n]
      (let [dur (:duration n)
            pos-val (->> n :position pos-val)
            med-tempo ((tempo-interpolator) pos-val (+ pos-val dur))]
        (to-ms dur med-tempo)))

    (defn pos-to-ms [p]
      (let [dur (pos-val p)
            med-tempo ((tempo-interpolator) 0 dur)]
        (to-ms dur med-tempo)))

;-----------------------------------------
;;;;;;;;;;;;;;; timetable ;;;;;;;;;;;;;;;;
;-----------------------------------------

  (defn make-time-map 
    ([] {})
    ([kyes] (a make-time-map (interleave kyes (repeat {0 nil}))))
    ([attr time-val-map & others]
      (let [v (map->sorted (map-keys to-num time-val-map))]
        (merge {attr v} (a make-time-map others)))))
    
  (defn get-at 
    ([tm at]
      (map-h (fn [k v] {k (get-at tm (to-num at) k)}) tm))
    ([tm at attr]
      (-> (filter #(>= (to-num at) (key %)) (attr tm))
            last second
            (or (val (first (attr tm)))))))

  (defn assoc-at 
    [tm at attr value]
    (assoc-in tm (conj (vec-if-not attr) (to-num at)) value))

  (defn time-val [m] 
    (let [tm (make-time-map :val m)]
      (with-type 'TimeVal
        (fn fun 
          ([] (:val tm))
          ([at] (get-at tm at :val))
          ([at value] (time-val (:val (assoc-at tm at :val value))))))))
      
  (defn time-map [& args] 
    (let [tm (a make-time-map args)]
      (with-type 'TimeMap 
        (fn fun 
          ([] tm)
          ([at] (get-at tm at))
          ([at attr] (get-at tm at attr))
          ([at attr value] (a time-map (a concat (assoc-at tm at attr value))))))))