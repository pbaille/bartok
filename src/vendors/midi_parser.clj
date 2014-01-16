(ns vendors.midi-parser
  (:use clojure.java.data)
  (:use vendors.debug-repl)
  (:use utils.utils)
  (:import (java.io File)
           (javax.sound.midi MidiSystem Sequence MidiMessage MidiEvent ShortMessage Track)))

(defn- note-on? [msg] (= (.getCommand msg) 0x90))
(defn- note-off? [msg] (= (.getCommand msg) 0x80))
(defn- note-msg? [msg] (or (note-on? msg)(note-off? msg))) 

(defn- on-off-coupling [ons-offs]
  (let [{ons :ons offs :offs} (group-by #(if (zero? (:velocity %)) :offs :ons) ons-offs)]
    (map (fn [{p :pitch pos :position v :velocity}
                 {pos-off :position}]
              (hash-map :pitch p :velocity v :position pos :duration (- pos-off pos))) 
         ons offs)))

(defn parse-track [resolution track] 
  ; (vendors.debug-repl/debug-repl)
  (loop [parsed []
         event-index 0]
    (let [event (.get track event-index)
          message (.getMessage event)]
      (cond
        (= (inc event-index) (.size track)) (on-off-coupling parsed)
        (not (instance? ShortMessage message)) 
          (recur parsed (inc event-index))
        (note-msg? message) 
          (recur
            (conj parsed 
                  {:pitch (.getData1 message)
                   :velocity (if (note-on? message) (.getData2 message) 0) 
                   :position (* (/ resolution 100)(.getTick event))})
            (inc event-index))
        :else (recur parsed (inc event-index))))))

(defn parse-midi-file [file-name] 
  (let [midi-seq (-> (File. file-name) MidiSystem/getSequence)
        resolution (.getResolution midi-seq)
        tracks (.getTracks midi-seq)
        cnt (-> tracks from-java count)]
    ; (debug-repl)
    (->> (for [n (range cnt)] {n (parse-track resolution (aget tracks n))})
         (a merge)
         (reduce #(if (seq (second %2))(conj %1 %2) %1) {}))))

;(parse-midi-file "src/midi-files/rmmlo.mid")

(defn merge-channels [chans]
  (a concat 
     (reduce (fn [acc [chan data]] 
               (conj acc (map #(assoc %1 :channel chan) data))) 
             [] chans)))

;***************************************************************************

; (defn add-note [msg notes] 
;   (let [k (.getData1 msg) 
;         v (.getData2 msg)] 
;     (if (> v 0) 
;       (assoc notes k 
;         (+ (.getData2 msg) (get notes k 0))) (dissoc notes k))))

; (defn parse-midi-file
;   ([file-name] 
;    (apply merge 
;       (for [chan (-> (File. file-name) MidiSystem/getSequence 
;                      .getTracks from-java count range)]
;      {(keyword (str chan)) (parse-midi-file file-name chan)})))
;   ([file-name track] 
;    (let [note-on 0x90
;          note-off 0x80
;          sequence (MidiSystem/getSequence (File. file-name))
;          track  (-> sequence .getTracks (aget track))]
;      (vendors.debug-repl/debug-repl)
;      (loop [current-notes {}
;             parsed []
;             last-time 0
;             event-index 0]
;        (let [event (.get track event-index)
;              message (.getMessage event)]
;          (cond
;            (= (inc event-index) (.size track)) parsed
;            (not (instance? ShortMessage message)) 
;              (recur current-notes parsed last-time (inc event-index))
;            (= (.getCommand message) note-on) 
;              (if (= (.getTick event) last-time)
;                (recur 
;                  (add-note message current-notes)
;                  parsed
;                  last-time
;                  (inc event-index))
;                (recur
;                  (add-note message current-notes)
;                  (conj parsed 
;                        {:sound current-notes 
;                         :duration (- (.getTick event) last-time)})
;                  (.getTick event)
;                  (inc event-index)))
;            (= (.getCommand message) note-off) 
;              (if (= (.getTick event) last-time)
;                (recur
;                  (dissoc current-notes (.getData1 message))
;                  parsed
;                  last-time
;                  (inc event-index))
;                (recur
;                  (dissoc current-notes (.getData1 message))
;                  (conj parsed 
;                        {:sound current-notes 
;                         :duration (- (.getTick event) last-time)})
;                  (.getTick event)
;                  (inc event-index)))
;            :else (recur current-notes parsed last-time (inc event-index))))))))