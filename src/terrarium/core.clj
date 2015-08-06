(ns terrarium.core
  (:require [ubergraph.core :as uber])
  (:require [frinj.jvm :refer (frinj-init!)])
  (:require [frinj.ops :refer (fj to)])
  (:gen-class))

(frinj-init!)

(defn make-port [type desc rate] {:type type
                                  :desc desc
                                  :rate rate})

(def make-input (partial make-port :input))
(def make-output (partial make-port :output))

(defn make-resource [name] {:name name})

(defn keyed
  "Turn a seq of maps into a map of maps, where the key of the new map equals the value at key"
  [k vs]
  (reduce (fn [a v] (assoc a (k v) v)) {} vs))

(def blocks
  (keyed :name
         [{:name :fishtank
           :ports (keyed :desc
                         [(make-input :fish-food (fj 8 :oz :per :day))
                          (make-input :clean-water (fj 50 :gal :per :day))
                          (make-output :fish (fj 60 :lb :per :year))
                          (make-output :poo-water (fj 50 :gal :per :day))])}
          {:name :plants
           :ports (keyed :desc
                         [(make-input :poo-water (fj 50 :gal :per :day))
                          (make-output :clean-water (fj 50 :gal :per :day))
                          (make-output :biomass (fj 360 :lb :per :year))])}
          {:name :jug
           :ports (keyed :desc
                         [(make-input :clean-water (fj 50 :gal :per :day))

                          ])}]))

(def all-ports (->> (vals blocks)
                    (map #(vals (get % :ports)))
                    (flatten)))

(defn block-ports
  ([block]
   (vals (:ports block)))
  ([block type]
   (filter #(= type (:type %)) (block-ports block))))

(def inputs #(block-ports % :input))
(def outputs #(block-ports % :output))

(defn get-port
  ([block-name input-name]
   (-> blocks
       (get block-name)
       (get :ports)
       (get input-name))))

(defn mk-connection
  [[block-out-name output-name] [block-in-name input-name] resource-name]
  (let [input (get-port block-in-name input-name)
        output (get-port block-out-name output-name)
        data {:resource resource-name}]
    [output input data]))

(def connections
  [(mk-connection [:fishtank :poo-water] [:plants :poo-water] :poo-water)
   (mk-connection [:plants :clean-water] [:fishtank :clean-water] :clean-water)
   (mk-connection [:jug :clean-water] [:fishtank :clean-water] :clean-water)])

(def graph (-> (uber/graph)
               (uber/add-nodes* all-ports)
               (uber/add-edges* connections)))

(defn by-name [coll name]
    (first (filter #(= name (get % :name)) coll)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
