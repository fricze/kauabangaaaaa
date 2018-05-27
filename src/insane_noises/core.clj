(ns insane-noises.core
  (:use [overtone.live])
  (:use [overtone.sc.machinery.synthdef])
  (:use [clojure.pprint])
  (:require
   [overtone.inst.drum :as drum]
   [overtone.inst.synth :as synth]
   [overtone.live :as overtone]
   [leipzig.live :as live]
   [leipzig.scale :as scale]
   [leipzig.melody :refer [all bpm is phrase tempo then times where with]]))

(def metro (metronome 110))

(definst saw-wave [freq 440 attack 0.03 sustain 0.4 release 0.3 vol 0.15]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(saw-wave 440)
(saw-wave (midi->hz 69))

(defn note->hz [music-note]
	(midi->hz (note music-note)))

(defn saw2 [music-note]
	(saw-wave (midi->hz (note music-note))))
(saw2 :A4)

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(synth/harmonic-swimming)

(synth/cs80lead 84)

(synth/ks-stringer)

(stop)

(def m (synth/mooger 63))

(kill m)

(definst bar [] (sin-osc [440 442 444 480 560]))

(bar)

(stop)

(defn hati-fnati [metro beat base]
  (at (metro (+ base 1/4 beat)) (drum/closed-hat))
  (at (metro (+ base 2/4 beat)) (drum/closed-hat))
  (at (metro (+ base 3/4 beat)) (drum/closed-hat))
  (at (metro (+ base 7/16 beat)) (drum/closed-hat)))

(defn player [beat]
  (at (metro beat) (drum/kick))

  (at (metro (+ 1 beat)) (drum/dub-kick))
  (at (metro (+ 2 beat)) (drum/dub-kick))
  (at (metro (+ 2 1/2 beat)) (drum/dub-kick))
  (at (metro (+ 3 beat)) (drum/dub-kick))
  (at (metro (+ 3 1/3 beat)) (drum/dub-kick))
  (at (metro (+ 3 2/3 beat)) (drum/dub-kick))
  (at (metro (+ 3 5/6 beat)) (drum/dub-kick))

  (at (metro (+ 1 beat)) (drum/snare 320 1 1/4))
  (at (metro (+ 3 beat)) (drum/snare 320 1 1/4))
  (at (metro (+ 1 7/16 beat)) (drum/snare 120 1/2 1/4))

  (hati-fnati metro beat 0)
  (hati-fnati metro beat 1)
  (hati-fnati metro beat 2)
  (hati-fnati metro beat 3)

  (apply-by (metro (+ 4 beat))
            #'player (+ 4 beat) []))

;; play
(player (metro))

(overtone.examples.compositions.jazz)

(stop)

;;This is an annotated version of this synth definition to help in understanding
;; some basics about synths and the synthdef data structure used in Overtone.

;; Here is a basic kick drum.  An envelope generator is multiplied by a low frequency
;; sin wave, which is like turning the volume knob up and down really quick while playing
;; a low tone.
(defsynth kick [amp 0.5 decay 0.6 freq 65]
  (let [env (env-gen (perc 0 decay) 1 1 0 1 FREE)
        snd (sin-osc freq (* Math/PI 0.5))]
    (out 0 (pan2 (* snd env amp) 0))))

;; This can be triggered using the name as a function call:
(kick)

;; A shorter variation using definst which allows you to leave out the out and pan ugens,
;; which get added by default to synths that are rooted by an audio rate ugen that isn't out.
(definst kick [amp 0.5 decay 0.6 freq 65]
  (* (sin-osc freq (* Math/PI 0.5))
     (env-gen (perc 0 decay) 1 1 0 1 FREE)
     amp))

;; Similarly this is also triggered using the name as a function call:
(kick)

;; The defsynth function will create a synthesizer definition structure that mimics the
;; binary format sent to the SuperCollider server.  These could also be created in other
;; ways, for example if you have in idea for a synthesizer DSL...

;; The structure is stored in each synth you create in a key called :sdef. So to retrieve
;; the structure of our kick synth we need to issue:
(:sdef kick)

;; This might be improved by pretty printing the result:
(synthdef-print (:sdef kick))

;; Here is the annotated output of (:sdef kick)
(pprint (:sdef kick))


(def melody
                                        ; Row,  row,  row   your  boat
  (phrase [3/3   3/3   2/3   1/3   3/3]
          [3/3   3/3   1/3   1/3   3/3]
          [  0     0     0     1     2]))

(overtone/definst beep [freq 440 dur 1.0]
  (-> freq
      overtone/saw
      (* (overtone/env-gen (overtone/perc 0.05 dur) :action overtone/FREE))))

(defmethod live/play-note :default [{midi :pitch seconds :duration}]
  (-> midi overtone/midi->hz (beep seconds)))

(def reply "The second bar of the melody."
  ;; Gent -ly  down the stream
  (phrase [2/3  1/3  2/3  1/3  6/3]
          [  2    1    2    3    4]))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
       (all :part :bass)))

(defmethod live/play-note :bass [{midi :pitch}]
  ;; Halving the frequency drops the note an octave.
  (-> midi overtone/midi->hz (/ 2) (beep 0.5)))

(->>
 melody
 (tempo (bpm 120))
 (where :pitch (comp scale/F scale/minor))
 live/play)

(->>
 bass
 (then (with bass melody))
 (then (with bass melody reply))
 (then (times 2 bass))
 (tempo (bpm 90))
 (where :pitch (comp scale/C scale/major))
 live/play)

(defmethod live/play-note :melody [{midi :pitch}]
  (some-> midi overtone/midi->hz beep))

(def boring-scale
  (->> (phrase (repeat 1) (range 8))
       (all :part :melody)
       (where :pitch (comp scale/G scale/blues))))

(live/jam (var boring-scale))

(stop)

;; scale/minor
