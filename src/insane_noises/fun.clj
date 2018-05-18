(ns insane-noises.fun)

(use '[mud.core])
(use '[overtone.core])
(require '[mud.timing :as time])

(mud.core/ctl-global-clock 8.)

;;set the rate
;; (ctl time/root-s :rate 8.)

;;Set timing controls on a Synth

;; defsample
;; defsynth

(defsynth seqer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 pattern 0  num-steps 8
   beat-bus (:count time/main-beat)     ;; Our beat count
   beat-trg-bus (:beat time/main-beat)  ;; Our beat trigger
   amp 0.7
   rate-start 0.1
   rate-limit 0.9]
  (let [cnt      (in:kr beat-bus)
        rander (mod cnt 1)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 pattern cnt)
                      (= beat-num (mod cnt num-steps))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out out-bus (* vol amp
                    (scaled-play-buf
                     :num-channels 2 :buf-num buf
                     :rate (t-rand:kr rate-start rate-limit rander) :trigger bar-trg)))))

(defonce kick-seq (buffer 256))
(doall (map #(do
               (println %1)
               (seqer :beat-num %1 :pattern kick-seq :num-steps 8 :buf (freesound-sample 194114)))
            [1 1 1 1]))

(pattern! kick-seq
          [1 1 1 1])

(stop)


(use '[mud.core])
(use '[mud.chords])
(use '[overtone.inst.sampled-piano])

overtone.inst.sampled-piano

;; (overtone.inst.sampled-piano/sampled-piano)


(def singing-chord-g
  (chord-synth overtone.inst.sampled-piano/sampled-piano 3
               :amp 0.0 :noise-level 0.05
               :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th)
               :attack 0.1 :release 0.1)

  )

mud.chords/chord-pattern

(:bufs   singing-chord-g) ;; Access all the bufs of a chord.
(:synths singing-chord-g) ;; Access all the running synths of a chord.

(chord-pattern singing-chord-g [[:C3 :E3 :F3]])

(ctl (:synths singing-chord-group-g) :amp 0.2)
