(ns bartok.harmony.chord-notation
  (:use utils.all)
  (:use [bartok primitives print])
  (:require [clojure.string :as s]))

(def ^:private known-chords-syns
 (reduce-kv
   #(a assoc %1 (mapcat (fn [x] (vector x %2)) %3))
   {}  
   {[:M3 :P5]    [:Major :major :Maj :maj :M]
    [:m3 :P5]    [:Minor :minor :Min :min :- :m]
    [:M3 :+5]    [:Aug :aug :+]
    [:m3 :b5]    [:Dim :dim :o]
    [:M3 :P5 :m7][:Dominant :dominant :Seventh :seventh :Dom :dom :7th :7]
    [:M3 :P5 :M7][:M7 :maj7 :Maj7 :major7 :Major7 :∆ :∆7]
    [:m3 :b5 :o7][:o7 :dim7 :Dim7]
    [:m3 :b5 :m7][:ø :Ø]}))

(def ^:private seventh-syns 
  (reduce-kv
   #(a assoc %1 (mapcat (fn [x] (vector x %2)) %3))
   {}  
   {:m7 [:Dominant :dominant :Seventh :seventh :Dom :dom :7th :7]
    :M7 [:M7 :maj7 :Maj7 :major7 :Major7 :∆ :∆7]
    :o7 [:o7 :dim7 :Dim7]}))

(def ^:private base-pat 
  (as>> (keys known-chords-syns)
        (map name)
        (sort-by count) ;sort for ensure good precedance
        reverse
        (interpose "|")
        (a str)
        (s/replace _ #"\+" "\\\\+") ;escape + chars
        (str "(" _ ")" "?");wrap and ?
        re-pattern))

(def ^:private seventh-pat
  #"(dominant|Dominant|seventh|Seventh|Major7|major7|Dim7|dim7|Maj7|maj7|7th|dom|Dom|o7|∆7|M7|∆|7)?")

(def ^:private cic-syns-pat
  #"(bb|o|b|m|M|N|P|#|\+|x|)(10|11|12|13|[1-9])|\+")

(def ^:private extensions-pat
  #"([[bb|o|b|m|M|N|P|#|\+|x|\-]?[10|11|13|[1-9]]|\+|sus|omit[2-7]]*)?")

(def ^:private ext-pat
  #"[bb|o|b|m|M|N|P|#|\+|x|\-]?[10|11|13|[1-9]]|\+|sus|omit[2-7]")

(def ^:private chord-notation-pat
  (pat-comp 
    base-pat 
    seventh-pat 
    extensions-pat))

(defn- add-cic [base cic]
  (conj (remove-nth base (-> cic b> :d-class :val inc)) cic))

(defn- cic-syns 
  [kw]
  (let [[_ alt n] (re-find cic-syns-pat (name kw))
        alt (when (seq alt) (alteration (keyword alt)))
        n (when n (mod (dec (parse-int n)) 7))]
    (cond 
      (not n) :+5
      (and (= 6 n)(not alt)) :m7 ; because 7 => m7
      :else (:name (c-interval-class 
                     (d-interval-class n) 
                     (or alt (alteration 0)))))))

(defn- remove-nth [base n] 
  (remove (f> b> :d-class :val (= (dec n))) base))

(defn- remove-third [base] (remove-nth base 3))

(defn- add-extensions 
  [base adds]
  (reduce 
    #(let [[ext pref n] (re-find #"([^\d]*)(\S*)" %2)]
       (cond
         (= pref "sus") (remove-third %1)
         (= pref "omit") (remove-nth %1 (parse-int n))
         :else (add-cic %1 (cic-syns ext))))
    base 
    (re-seq ext-pat (name adds))))

(defn parse-chord 
  "given a chord notation return a seq of c-interval-class
  ex: (parse-chord :69) => (:M2 :P5 :M3 :M6)
      (parse-chord :m∆9) => (:M2 :P5 :M7 :m3)
      (parse-chord :sus2) => (:M2 :P5)"
  [kw]
  (let [[_ base seventh extensions] (re-find chord-notation-pat (name kw)) ;(s/replace (name kw) #"\." "")
        base (if base (known-chords-syns (keyword base)) [:M3 :P5]) ;defaults to Major
        base (if seventh (conj base (seventh-syns (keyword seventh))) base)]; add seventh
    (add-extensions base extensions)))


