(ns bartok.harmony.chord-notation
  (:use utils.all)
  (:use bartok.primitives)
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
  #"(bb|o|b|m|M|N|P|#|\+|x|\.|)(10|11|12|13|[1-9])|\+")

(def ^:private extensions-pat
  #"([[bb|o|b|m|M|N|P|#|\+|x|\-|\.]?[10|11|13|[2-9]]|\+|sus|omit[2-7]]*)?")

(def ^:private ext-pat
  #"(bb|o|b|m|M|N|P|#|\+|x|\-|\.)?(10|11|13|[2-9])|(\+)|(sus)|(omit)([2-7])")

(def ^:private chord-notation-pat
  (re-cat 
    base-pat 
    seventh-pat 
    extensions-pat))

(defn- cic-syns
  "do this best to return a c-interval-class given a compatible named object
  ex: (cic-syns :+3) => :M3
      (cic-syns :#5) => :+5
      (cic-syns :13) => :M6" 
  [kw]
  (let [[_ alt n] (re-find cic-syns-pat (name kw))
        alt (when (and (seq alt) (not= alt ".")) (alteration (keyword alt)))
        n   (when n (mod (dec (parse-int n)) 7))]
    (cond 
      (and (not n)(= alt "+")) :+5
      (and (not n)(not alt)) nil
      (and (= 6 n)(not alt)) :m7 ; because 7 => m7
      :else (:name (c-interval-class 
                    (d-interval-class n) 
                    (or alt (alteration 0)))))))

(defn- remove-nth 
  "remove corresponding 'd-interval-class compatible' c-interval-class from the base
  ex: (remove-nth [:M3 :P5] 3) => (:P5)"
  [base n] 
  (remove (f> b> :d-class :val (= (dec n))) base))

(defn- remove-third [base] (remove-nth base 3))

(defn- add-cic 
  "add a degree(c-interval-class) to a collection of degrees 
  while removing previous occurence of the same d-interval-class
  ex: (add-cic [:m2 :P5] :+5) => (:m2 :+5)"
  [base cic]
  (sort-by (f> b> :val)
    (conj (remove-nth base (-> cic b> :d-class :val inc)) cic)))

(defn- full-name 
  "when a keyword contains a '/', name function return only the right part.
  this function will return the full name"
  [kw]
  (if (seq (str (namespace kw))) 
    (str (symbol (str (namespace kw)) (name kw))) 
    (name kw)))

(defn- add-extensions 
  "split extensions and apply them to the base"
  [base adds]
  (reduce 
     #(let [[ext pref n] (re-find #"([^\d]*)(\S*)" %2)]
       (cond
         (= pref "sus") (remove-third %1)
         (= pref "omit") (remove-nth %1 (parse-int n))
         :else (add-cic %1 (cic-syns ext))))
    base 
    (map first (re-seq ext-pat (name adds)))))

(defn parse-chord-class 
  "given a chord-class notation return a seq of c-interval-class
  ex: (parse-chord-class :69) => (:M2 :P5 :M3 :M6)
      (parse-chord-class :m∆9) => (:M2 :P5 :M7 :m3)
      (parse-chord-class :sus2) => (:M2 :P5)"
  [kw]
  (let [[_ base seventh extensions] (re-find chord-notation-pat (name kw)) 
        base (if base (known-chords-syns (keyword base)) [:M3 :P5]) ;defaults to Major
        base (if seventh (conj base (seventh-syns (keyword seventh))) base)]; add seventh
    (add-extensions base extensions)))

(declare parse-chord)

(defn- add-bass 
  "for foreign bass chords like C/E (Ctriad over D bass)"
  [chord bass]
  (let [bass-cic (:name (c-interval-class (:root chord) bass))
        pcs (when-not (in? (:pitch-classes chord) bass) 
              (cons bass (:pitch-classes chord)))]
    (-> chord
        (assoc :bass bass)
        (update-in [:degrees] add-cic bass-cic)
        (update-in [:pitch-classes] #(or pcs %)))))

(defn- merge-superpos 
  "for superposition chord symbols like C|B (Ctriad over Btriad)"
  [chord superpos]
  (let [sup (parse-chord superpos)
        base-sup-inter (c-interval-class (:root sup) (:root chord))
        trans-degrees (map #(:name (b:+ base-sup-inter %)) (conj (:degrees chord) :P1))
        merged-degs (reduce add-cic (:degrees sup) trans-degrees)
        root (:root sup)
        pcs (cons root (map #(:name (transpose root %)) merged-degs))]
    {:root root :degrees merged-degs :pitch-classes pcs :bass (:bass sup)}))

(comment 
  (parse-chord :C69) 
  (parse-chord :Fm∆9) 
  (parse-chord :Absus2)
  (parse-chord :C/B) 
  (parse-chord :C|B) 
  (parse-chord :C|B/D#))

(defn parse-chord
  "given a chord notation return a map {:root _ :degrees _ :pitch-classes _}
  ex: (parse-chord :C69) => {:root :C, :degrees (:M2 :M3 :P5 :M6), :pitch-classes (:C :D :E :G :A)}
      (parse-chord :Fm∆9) => {:root :F, :degrees (:M2 :m3 :P5 :M7), :pitch-classes (:F :G :Ab :C :E)}
      (parse-chord :Absus2) => {:root :Ab, :degrees (:M2 :P5), :pitch-classes (:Ab :Bb :Eb)}
      (parse-chord :C/B) => {:root :C, :degrees (:M7 :M3 :P5), :pitch-classes (:B :C :E :G), :bass :B}
      (parse-chord :C|B) => {:root :B, :degrees (:m2 :M3 :P4 :P5 :m6), :pitch-classes (:B :C :D# :E :F# :G), :bass :B}
      (parse-chord :C|B/D#) => {:root :B, :degrees (:m2 :M3 :P4 :P5 :m6), :pitch-classes (:B :C :D# :E :F# :G), :bass :D#}"
  [x]
  (let [full-name       (full-name x)
        [base bass]     (s/split full-name #"/")
        [base superpos] (s/split full-name #"\|")
        [_ root class]  (re-find #"([A-G][#|b|bb|x]?)(\S*)?" base)
        root            (keyword root)
        degrees         (parse-chord-class (or class "Maj"))
        pitch-classes   (cons root (map :name (map (p transpose root) degrees)))
        chord           {:root root :degrees degrees :pitch-classes pitch-classes :bass root}]
    (cond 
      superpos (merge-superpos chord (keyword superpos))
      bass (add-bass chord (keyword bass))
      :else chord)))

