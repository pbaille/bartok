(ns bartok.midi.parser
  (:use clojure.java.data)
  (:use utils.utils)
  (:use vendors.debug-repl)
  (:import (java.io File)
           (java.util Arrays)
           (java.nio ByteBuffer ByteOrder)
           (javax.sound.midi MidiSystem Sequence MidiMessage MidiEvent ShortMessage MetaMessage Track)))
 
(defn- note-on?        [msg] (or= (.getCommand msg) (range 0x90 0xA0)))
(defn- note-off?       [msg] (or= (.getCommand msg) (range 0x80 0x90)))
(defn- poly-after?     [msg] (or= (.getCommand msg) (range 0xA0 0xB0)))
(defn- control-change? [msg] (or= (.getCommand msg) (range 0xB0 0xC0)))
(defn- program-change? [msg] (or= (.getCommand msg) (range 0xC0 0xD0)))
(defn- chan-after?     [msg] (or= (.getCommand msg) (range 0xD0 0xE0)))
(defn- pitch-wheel?    [msg] (or= (.getCommand msg) (range 0xE0 0xF0)))

(defn- note-msg? [msg] (or (note-on? msg)(note-off? msg))) 

(defn- tempo-msg?          [msg] (= (.getType msg) 0x51))
(defn- time-signature-msg? [msg] (= (.getType msg) 0x58))
(defn- key-signature-msg?  [msg] (= (.getType msg) 0x59))

(def int->key 
  {0 :C -1 :Bb -2 :Bb -3 :Eb -4 :Ab 
   -5 :Db -6 :Gb -7 :Cb 1 :G 2 :D 
   3 :A 4 :E 5 :B 6 :F# 7 :C#})

; (defn- bpm-at [midi-pos parsed-file] ())

(defn- valid-meta-msg? [msg] 
  (or-> msg
        tempo-msg?         
        time-signature-msg?
        key-signature-msg? ))

(defn- valid-msg? [msg] 
  (or-> msg
        note-on?
        note-off?      
        ;poly-after?    
        ;control-change? 
        ;program-change? 
        ;chan-after?    
        ;pitch-wheel?
        ))

(defn meta-msg-of-type [t m] (with-meta m {:type t :msg-type :meta}))
(defn midi-msg-of-type [t m] (with-meta m {:type t :msg-type :midi}))

(defn- parse-meta-message [msg tick]
  (cond
    (tempo-msg? msg)  
      (meta-msg-of-type :tempo
        {:position tick
         :bpm (->> (a format "0x%x%x%x" (.getData msg)) 
                   read-string 
                   (/ 60000000) 
                   float 
                   (round 1))})        
    (time-signature-msg? msg)
      (meta-msg-of-type :time-signature
        {:position tick
         :signature (let [[n d] (.getData msg)]
                      [n (int (clojure.contrib.math/expt 2 d))])}) 
    (key-signature-msg? msg)
      (meta-msg-of-type :key-signature
        {:position tick
         :key (get int->key (first (.getData msg)))})
    :else nil))

(defn- parse-message [msg tick]
  (cond
    (note-msg? msg) 
      (midi-msg-of-type :note
        {:channel  (.getChannel msg)
         :pitch    (.getData1 msg)
         :velocity (if (note-on? msg) (.getData2 msg) 0) 
         :position tick})
    (poly-after? msg)
      (midi-msg-of-type :polyphonic-aftertouch
        {:channel  (.getChannel msg)
         :data     [(.getData1 msg)(.getData2 msg)]
         :position tick})
    (control-change? msg)
      (midi-msg-of-type :control-change
        {:channel  (.getChannel msg)
         :data     [(.getData1 msg)(.getData2 msg)]
         :position tick})
    (program-change? msg)
      (midi-msg-of-type :program-change
        {:channel  (.getChannel msg)
         :data     (.getData    msg)
         :position tick})
    (chan-after? msg)
      (midi-msg-of-type :channel-aftertouch
        {:channel  (.getChannel msg)
         :data     (.getData    msg)
         :position tick})
    (pitch-wheel? msg)
      (midi-msg-of-type :pitch-wheel
        {:channel  (.getChannel msg)
         :data     (.getData    msg)
         :position tick})
    :else nil))

(defn- time-format [resolution track]
  (map (fn [event] 
         (let [pos (/ (:position event) resolution)
               event (assoc event :position pos)]
           (if (:duration event)
             (update-in event [:duration] / resolution)
             event))) 
       track))

;grab all note-on and note-off message and couple them into :note type with duration
(defn- on-off-coupling [track]
    ; (debug-repl)
    (let [{notes :note :as by-type} (group-by type track)
          {ons :ons offs :offs} (group-by #(if (zero? (:velocity %)) :offs :ons) notes)
           coupled (map (fn [{pos-on :position :as m} {pos-off :position}]
                          (assoc m :duration (- pos-off pos-on))) 
                        ons offs)]
        (->> (assoc by-type :note coupled) vals (a concat))))

(defn  split-msg-type [track] (group-by #(:msg-type (meta %)) track))

(defn- merge-meta [track]
  (let [{metas :meta midis :midi} (split-msg-type track)]
    (if-let 
      [mmsgs (->> metas 
               (group-by type) 
               (map-vals #(map vals %))
               (map-vals #(->> % (a concat) (a hash-map)))
               (with-type :meta-messages))]
      (if (seq midis) (conj midis mmsgs) [mmsgs])
      midis)))

(defn- parse-track [track] 
  ; (vendors.debug-repl/debug-repl)
  (loop [parsed []
         event-index 0]
    (let [event (.get track event-index)
          tick (.getTick event)
          message (.getMessage event)]
      (cond
        (= (inc event-index) (.size track)) parsed
        (and (instance? MetaMessage message) (valid-meta-msg? message))
          (recur (conj parsed (parse-meta-message message tick)) (inc event-index))
        (and (instance? ShortMessage message) (valid-msg? message)) 
          (recur (conj parsed (parse-message message tick)) (inc event-index))
        :else (recur parsed (inc event-index))))))

;main
(defn parse-midi-file [file-name] 
  (let [midi-seq (-> (File. file-name) MidiSystem/getSequence)
        tracks (.getTracks midi-seq)
        res (.getResolution midi-seq)
        cnt (-> tracks from-java count)]
    ; (debug-repl)
    (->> (for [n (range cnt)] 
           (->> (parse-track (aget tracks n))
                on-off-coupling
                (time-format res)
                merge-meta))
          (remove empty?))))

;(parse-midi-file "src/midi-files/rmmlo.mid")

(defn filter-msg-type [type-kw parsed-midi-file]
  (filter #(= (type %) type-kw) parsed-midi-file))





; (-> (byte-array [0x41 0xA0 0x00 0x00]) ByteBuffer/wrap (.order (ByteOrder/nativeOrder)) .getFloat)

;ns-name ns-aliases ns-imports ns-interns ns-map ns-publics ns-refers
;float f = ByteBuffer.wrap( array ).order( ByteOrder.nativeOrder() ).getFloat();  
