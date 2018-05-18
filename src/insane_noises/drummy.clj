(ns insane-noises.drummy
  (:use [overtone.live])
  (:use [overtone.sc.machinery.synthdef])
  (:use [clojure.pprint])
  (:require
   [overtone.inst.drum :as drum]
   [overtone.inst.sampled-piano :as piano]
   [overtone.live :as overtone]
   [leipzig.live :as live]
   [leipzig.scale :as scale]
   [leipzig.melody :refer [all bpm is phrase tempo then times where with]]))

(def metro (metronome 120))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))


(definst o-hat [amp 0.8 t 0.5]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(defn swinger [beat]
  (at (metro beat) (overtone.inst.drum/kick2))

  (at (metro (+ 1 beat)) (overtone.inst.drum/snare2 50))

  ;; (at (metro (+ 1/2 beat)) (overtone.inst.drum/closed-hat 0.3))
  (at (metro (+ 1/3 beat)) (overtone.inst.drum/closed-hat 0.3))
  (at (metro (+ 1/10 3/4 beat)) (overtone.inst.drum/closed-hat 0.3))

  ;; (at (metro (+ 1 1/10 beat)) (overtone.inst.drum/snare))
  ;; (at (metro (+ 1 3/4 beat)) (overtone.inst.drum/soft-hat))

  (apply-at (metro (+ 2 beat)) #'swinger (+ 2 beat) []))

(stop)

(swinger (metro))
