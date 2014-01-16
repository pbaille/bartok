(ns bartok.midi.parser
  (:use clojure.java.data)
  (:use vendors.debug-repl)
  (:use utils.utils)
  (:import (java.io File)
           (java.util Arrays)
           (java.nio ByteBuffer)
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

(defn- valid-meta-msg? [msg] 
  (or-> msg
        tempo-msg?         
        time-signature-msg?
        key-signature-msg? ))

(defn- valid-msg? [msg] 
  (or-> msg
        note-on?
        note-off?      
        poly-after?    
        control-change? 
        program-change? 
        chan-after?    
        pitch-wheel?))

(defn- parse-meta-message [msg tick]
  (cond
    (tempo-msg? msg)  
      {:type :tempo
       :data (from-java (.getData msg)) ;(.getShort (ByteBuffer/wrap (.getData msg)))
       :position tick}        
    (time-signature-msg? msg)
      {:type :time-signature
       :data     (from-java (.getData msg))
       :position tick} 
    (key-signature-msg? msg)
      {:type :key-signature
       :data     (from-java (.getData msg))
       :position tick}
    :else nil))

(defn- parse-message [msg tick]
  (cond
    (note-msg? msg) 
      {:type :note
       :channel  (.getChannel msg)
       :pitch    (.getData1 msg)
       :velocity (if (note-on? msg) (.getData2 msg) 0) 
       :position tick}
    (poly-after? msg)
      {:type :poly-after
       :channel  (.getChannel msg)
       :data     [(.getData1 msg)(.getData2 msg)]
       :position tick}
    (control-change? msg)
      {:type :control-change
       :channel  (.getChannel msg)
       :data     [(.getData1 msg)(.getData2 msg)]
       :position tick}
    (program-change? msg)
      {:type :program-change
       :channel  (.getChannel msg)
       :data     (.getData    msg)
       :position tick}
    (chan-after? msg)
      {:type :chan-after
       :channel  (.getChannel msg)
       :data     (.getData    msg)
       :position tick}
    (pitch-wheel? msg)
      {:type :pitch-wheel
       :channel  (.getChannel msg)
       :data     (.getData    msg)
       :position tick}
    :else nil))

(defn- on-off-coupling [data]
    (let [{notes :note :as by-type} (group-by :type data)
          {ons :ons offs :offs} (group-by #(if (zero? (:velocity %)) :offs :ons) notes)
          coupled (map (fn [{p :pitch pos :position v :velocity c :channel}
                            {pos-off :position}]
                        (hash-map :type :note :channel c :pitch p :velocity v :position pos :duration (- pos-off pos))) 
                       ons offs)]
        (->> (assoc by-type :note coupled) vals (a concat))))

;(ByteBuffer/wrap (byte-array (drop 3 (from-java (.getMessage message)))))
;(ByteBuffer/wrap (Arrays/copyOfRange (.getMessage message) 2 5))

(defn parse-track [track] 
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

; (defn apply-tempo [chans]
;   (map-vals (fn [data] 
;                (map #(assoc %1 :position (* 2(:position %1)) :duration (* 2(:duration %1))) data)) 
;              chans))

(defn parse-midi-file [file-name] 
  (let [midi-seq (-> (File. file-name) MidiSystem/getSequence)
        tracks (.getTracks midi-seq)
        cnt (-> tracks from-java count)]
    ; (debug-repl)
    (->> (for [n (range cnt)] 
           (parse-track (aget tracks n)))
         (mapcat on-off-coupling))))

;(parse-midi-file "src/midi-files/rmmlo.mid")

(defn filter-msg-type [type-kw parsed-midi-file]
  (filter #(= (:type %) type-kw) (parse-midi-file "src/midi-files/jeuxdeau.mid")))


