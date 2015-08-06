(ns terrarium.core
  (:require [ubergraph.core :as uber])
  (:require [frinj.jvm :refer (frinj-init!)])
  (:require [frinj.ops :refer (fj to)])
  (:require [terrarium.util :refer (keyed)])
  (:gen-class))

(frinj-init!)

(defrecord Block [name])
(defrecord Port [type block desc rate])
(defrecord Resource [name])

(def blocks (map ->Block [:fishtank :plants :jug]))

(def ports
  (let [B (partial get (keyed :name blocks))
        in (partial ->Port :input)
        out (partial ->Port :output)]
    [(in (B :fishtank) :fish-food (fj 8 :oz :per :day))
     (in (B :fishtank) :clean-water (fj 50 :gal :per :day))
     (out (B :fishtank) :fish (fj 60 :lb :per :year))
     (out (B :fishtank) :poo-water (fj 50 :gal :per :day))

     (in (B :plants) :poo-water (fj 50 :gal :per :day))
     (out (B :plants) :clean-water (fj 50 :gal :per :day))
     (out (B :plants) :biomass (fj 360 :lb :per :year))

     (out (B :jug) :clean-water (fj 50 :gal :per :day))]))

(defn get-port
  ([block-name port-name]
   (->> ports
        (filter #(and (= block-name (get-in % [:block :name])) (= port-name (:desc %))))
        (first))))

(defn mk-connection
  [[block-out-name output-name] [block-in-name input-name] resource-name]
  (let [input (get-port block-in-name input-name)
        output (get-port block-out-name output-name)
        data (->Resource resource-name)]
    [output input data]))

(def connections
  [(mk-connection [:fishtank :poo-water] [:plants :poo-water] :poo-water)
   (mk-connection [:plants :clean-water] [:fishtank :clean-water] :clean-water)
   (mk-connection [:jug :clean-water] [:fishtank :clean-water] :clean-water)])

(def graph (-> (uber/graph)
               (uber/add-nodes* ports)
               (uber/add-edges* connections)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
