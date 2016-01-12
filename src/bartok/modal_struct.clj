(in-ns 'bartok.primitives)

  (use 'clojure.set)

;;;;;;;;;;;;;;;;;;; MSC Work ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (def empty-msc*
    (with-type 'ModalStructClass 
     (map->sorted 
        (zipmap [:2nd :3rd :4th :5th :6th :7th] 
                (repeat nil)))))
    
  (defn msc-degrees 
    "return seq of all 'degrees' (c-interval-class) 
     of the given modal-struct-class "
    [msc]
    (vals (dissoc-nils msc)))
  
  (b-fn msc-assoc* 
    "assoc a degree to msc, overiding previous degree if needed"
    [msc & cics]
    {:pre [(type= msc 'ModalStructClass)
           (match-type? ['CIntervalClass] cics)]}
    (a assoc msc 
     (mapcat #(vector (-> % :d-class :name) %) 
             cics)))
  
  (b-fn msc-dissoc* 
    "dissoc given degrees, assoc nil to their keys, 
    'cics' can be either cic(s) or dic(s) or mixed"
    [msc & cics]
    {:pre [(type= msc 'ModalStructClass)]}
    (a assoc msc 
      (mapcat 
       #(cond 
          (type= % 'DIntervalClass) [(:name %) nil]
          (type= % 'CIntervalClass) [(-> % :d-class :name) nil]
          (number? %) [(:name (d-interval-class (dec %))) nil]
          :else (throw (Exception. (str "don't know how to dissoc : " % " .")))) 
       cics)))
  
  (declare litteral->msc)
  
  (b-fn modal-struct-class* 
    "modal-struct-class constructor, see below alias 'msc*'"
    ; conflict with mode-class litterals... 
    ;but mode and mode-class will be certainly removed
    ([x] 
     (cond 
       (coll? x)  (a modal-struct-class* x)
       (named? x) (litteral->msc x)))
    ([x & xs] (a msc-assoc* empty-msc* x xs)))
  
  (def msc* modal-struct-class*)
  
  (defn msc-rotations 
    "return a seq of all posible rotation of a given msc"
    [msc]
    (let [degs (conj (msc-degrees msc) (b> :P1))]
      (map
        (fn [d] 
          (msc* (map #(b:- % d) (remove (p = d) degs)))) 
        degs)))
  
  (def greek-modes 
    (zipmap [:Lyd :Mix :Eol :Loc :Ion :Dor :Phry]
            (msc-rotations (msc* :M2 :M3 :+4 :P5 :M6 :M7))))
  
  (def modes*
    (merge greek-modes
      (zipmap [:Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6] 
              (msc-rotations (msc* :M2 :M3 :+4 :+5 :M6 :M7)))
      (zipmap [:Lyd#2 :AltDim :Harmm :Loc6 :Ion+ :Dor+4 :PhryM]
              (msc-rotations (msc* :#2 :M3 :+4 :P5 :M6 :M7)))))
  
  (b-fn msc-in? 
    "check if given cics or dics are in msc"
    [msc & xs]
    (every? 
      #(cond
         (type= % 'CIntervalClass) (= ((-> % :d-class :name) msc) %)
         (type= % 'DIntervalClass) ((:name %) msc))
      xs))
  
  (defn msc-difference [x y]
    (difference 
      (set (msc-degrees x)) 
      (set (msc-degrees y))))
  
  (defn msc-intersection [x y]
    (intersection 
      (set (msc-degrees x)) 
      (set (msc-degrees y))))
    
  (defn msc-subset? [x y]
    (subset? 
      (set (msc-degrees x)) 
      (set (msc-degrees y))))
  
  (defn msc-complete? 
    "check if all degrees are present, complete mode"
    [msc]
    (count= (msc-degrees msc) 6))
  
  (defn modal-origins 
    "find the possible modal origins of an msc"
    [msc]
    (filter
      #(msc-subset? msc (val %))
      modes*))
  
  (def ^:private triads-litterals
    {:M [:M3 :P5] 
     :m [:m3 :P5]
     :o [:m3 :b5]
     :+ [:M3 :+5]})
  
  (def ^:private tetrads-litterals
    {:∆  [:M3 :P5 :M7]
     :+∆ [:M3 :+5 :M7]
     :m∆ [:m3 :P5 :M7]
     :o∆ [:m3 :b5 :M7]
     :7  [:M3 :P5 :m7]
     :m7 [:m3 :P5 :m7]
     :+7 [:M3 :+5 :m7]
     :o7 [:m3 :b5 :o7]
     :ø  [:m3 :b5 :m7]
     :6  [:M3 :P5 :M6]
     :m6 [:m3 :P5 :M6]})
  
  (def ^:private sus-triads-litterals
    {:M  [:P5]
     :+  [:+5]
     :b5 [:b5]})

  (def ^:private sus-tetrads-litterals
    {:∆  [:P5 :M7]
     :+∆ [:+5 :M7]
     :o∆ [:b5 :M7]
     :7  [:P5 :m7]
     :+7 [:+5 :m7]
     :o7 [:b5 :o7]
     :ø  [:b5 :m7]
     :6  [:P5 :M6]})
  
  (def ^:private superstruct-litterals
    {:b9  :m2
     :9   :M2
     :#9  :#2
     :b11 :b4
     :11  :P4
     :#11 :+4
     :b13 :m6
     :13  :M6
     :#13 :#6})
  
  (defn- val->key 
    "given a val and a map, return corresponding key"
    [m v]
    (get (map-invert m) v))
  
  (defn- msc-base 
    "find the base name of an msc, 
    and eventual omission notation like sus or omit5,
    return vector of [base-name remaining-degrees omission-kw]"
    [msc]
    (let [fifth (:name (:5th msc)) 
          third (:name (:3rd msc)) 
          sev-or-six (or (:name (:7th msc)) 
                         (when (msc-in? msc :M6) :M6))]
     (cond 
       (and third fifth sev-or-six) 
         (if-let [n (val->key tetrads-litterals [third fifth sev-or-six])]
           [n (msc-degrees (msc-dissoc* msc third fifth sev-or-six))])
       (and third fifth) 
         (if-let [n (val->key triads-litterals [third fifth])]
           [n (msc-degrees (msc-dissoc* msc third fifth))])
       (and third sev-or-six) 
         (if-let [n (val->key tetrads-litterals [third :P5 sev-or-six])]
           [n (msc-degrees (msc-dissoc* msc third sev-or-six)) :omit5])
       (and fifth sev-or-six) 
         (if-let [n (val->key sus-tetrads-litterals [fifth sev-or-six])]
           [n (msc-degrees (msc-dissoc* msc fifth sev-or-six)) :sus])
       fifth
         (if-let [n (val->key sus-triads-litterals [fifth])]
           [n (msc-degrees (msc-dissoc* msc fifth)) :sus]))))
  
  (defn- sparse-msc-name 
    "find a name for a given 'incomplete' msc"
    [msc]
    (let [[base-name adds omit-or-sus] (msc-base msc)
          sus   (when (= :sus omit-or-sus) :sus)
          omit5 (when (= :omit5 omit-or-sus) :omit5)
          adds  (a kwcat (map #(val->key superstruct-litterals (:name %)) adds))]
      (kwcat base-name sus adds omit5)))
  
  (defn msc-name 
    "find a name for an msc"
    [msc]
    (let [complete? (msc-complete? msc)
          origins (when complete? (modal-origins msc))]
      (cond 
        (seq origins)
          (ffirst origins)
        complete? 
          (let [diffs-map (map-vals (p msc-difference msc) modes*)
                [base diffs] (first (sort-by #(count (val %)) diffs-map))]
            (a kwcat base (map :name diffs)))
        :else 
          (sparse-msc-name msc))))
  
;;;;;;;;;;;;;;;;;; msc-parse ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require '[utils.regex :as re])

  (def ^:private msc-base-pat
    (re-cat
     (re/re-wrap
      (re-cat 
       (re/coll->or-pat (keys modes*))
       #"|"
       (re/coll->or-pat
        (concat (keys tetrads-litterals)
                (keys triads-litterals)))
       #"|"))
     #"(.*)"))

  (def ^:private add-pat
    #"(?:\.)?(?:bb|o|b|m|M|N|P|#|\+|x|\-)?(?:10|11|13|[2-9])|\+|sus|omit[2-7]")

  (def ^:private adds-pat
    (-> add-pat re/re-wrap (re-cat "*")))

  (def ^:private cic-syns-pat
    #"(\.)?(bb|o|b|m|M|N|P|#|\+|x)?(10|11|12|13|[1-9])?")

  (defn- cic-syns
    "do this best to return a c-interval-class given a compatible named object
    ex: (cic-syns :+3) => :M3
        (cic-syns :#5) => :+5
        (cic-syns :13) => :M6" 
    [kw]
    (let [[_ _ alt n] (re-find cic-syns-pat (name kw))
          alt (when alt (alteration (keyword alt)))
          n   (when n (mod (dec (parse-int n)) 7))]
      (cond 
        (and (not n) alt) :+5
        (and n (not alt)) (c-interval-class (d-interval-class n))
        n (:name (c-interval-class (d-interval-class n) alt))
        :else nil)))

  (defn- msc-assoc-adds 
    "take a msc and a 'adds' string, 
    return msc with adds applied"
    [msc adds]
    (reduce 
      #(let [addkw (keyword %2)
             syn (cic-syns %2)
             omit (when-let [[_ n] (re-matches #"(?:\.)?(?:omit)([2-7])" %2)]
                    (parse-int n))]
         (cond 
           (= (b-type addkw) 'c-interval-class)
             (msc-assoc* %1 addkw)
           (= addkw :sus) (msc-dissoc* %1 :3rd)
           omit (msc-dissoc* %1 omit)
           syn  (msc-assoc* %1 syn)
           :else %1))
      msc
      adds))
  
  (defn litteral->msc 
    "take a keyword or a string that represent a msc 
     an return the corresponding msc"
    [kw-or-str]
    (let [[all base adds] (re-matches msc-base-pat (name kw-or-str))
          adds (when (and all (re-matches adds-pat adds)) (re-seq add-pat adds))]
      (if (or base adds)
        (let [base-msc
              (if-let [base* ((keyword base) (merge tetrads-litterals triads-litterals))]
                (msc* base*)
                (if-let [msc-base* ((keyword base) modes*)] 
                  msc-base*
                  (msc* :M3 :P5)))
              msc (msc-assoc-adds base-msc adds)]
          msc))))

; (litteral->msc :m∆#11omit5)

;;;;;;;;;;;;;;; prio work ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn deg-occs 
    "takes a collection of mscs and return a map of type {CIntervalClass Number} 
    where Number is the occurence of corresponding CIntervalClass in all mscs"
    [mscs] 
    (map-vals 
      count 
      (group-by 
        identity 
        (mapcat msc-degrees mscs))))

  (defn rarest-degree 
    "return the rarest degree of a msc comparing with given mscs"
    [msc mscs]
    (let [deg-occs (deg-occs mscs)]
     (first 
      (sort-by #(get deg-occs %) 
               (msc-degrees msc)))))
  
  (defn prio 
    "return the caracteristics degrees of an msc comparing with given mscs"
    ([msc] (prio msc (vals modes*)))
    ([msc mscs]
    (loop [ret [(rarest-degree msc mscs)]
           top-classes (map second (modal-origins (msc* ret)))]
      (let [ret (conj ret (rarest-degree msc top-classes))
            top-classes (map second (modal-origins (msc* ret)))]
        (if (all-distinct? ret)
          (recur ret top-classes)
          (butlast ret))))))
 
  (prio (msc* :Lyd#6))
