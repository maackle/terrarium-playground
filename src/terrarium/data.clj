(ns terrarium.data
  (:require [ubergraph.core :as uber])
  (:require [frinj.ops :refer (fj to)])
  (:require [terrarium.model :refer :all])
  (:require [terrarium.util :refer (keyed)]))

(def blocks (map ->Block [:fishtank :plants :jug]))

(def ports
  (let [B (partial get (keyed :name blocks))
        in (partial ->Port :input)
        out (partial ->Port :output)]
    [(in  (B :fishtank) :fish-food (fj 8 :oz :per :day))
     (in  (B :fishtank) :clean-water (fj 50 :gal :per :day))
     (out (B :fishtank) :fish (fj 60 :lb :per :year))
     (out (B :fishtank) :poo-water (fj 40 :gal :per :day))

     (in  (B :plants) :poo-water (fj 50 :gal :per :day))
     (out (B :plants) :clean-water (fj 40 :gal :per :day))
     (out (B :plants) :biomass (fj 360 :lb :per :year))

     (out (B :jug) :clean-water (fj 500 :gal :per :day))]))

(def resources (map ->Resource [:poo-water
                                :clean-water]))

(def connections
  (mk-connections ports resources
                  [[[:fishtank :poo-water] :poo-water [:plants :poo-water]]
                   [[:plants :clean-water] :clean-water [:fishtank :clean-water]]
                   [[:jug :clean-water] :clean-water [:fishtank :clean-water]]]))

(def graph (build-graph ports resources connections))

(def accounts [(->Account :clean-water (fj 0 :gal))
               (->Account :poo-water (fj 0 :gal))])

(def initial-state {:graph graph
                    :accounts accounts})

(def state (atom initial-state))
