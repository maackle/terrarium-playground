(ns terrarium.model
  (:use clojure.pprint)
  (:require [ubergraph.core :as uber])
  (:require [frinj.jvm :refer (frinj-init!)])
  (:require [terrarium.util :refer (keyed)]))

(frinj-init!)

(defrecord Block [name])
(defrecord Port [type block desc rate])
(defrecord Resource [name])
(defrecord Account [name amount])


(defn get-port
  "Extract a port from a list by looking up block name and port name"
  ([ports block-name port-name]
   (->> ports
        (filter #(and (= block-name (get-in % [:block :name])) (= port-name (:desc %))))
        (first))))

(defn stuff [x] (do (println x) x))

(defn mk-connections [ports resources connections]
  "Create a list of connection definitions that can be fed to an uber/graph
  `ports` and `resources` are lists
  `connections` takes the following form:
    [[[:block-a :port-a] :connecting-resource [:block-b :port-b]]
     [[...] ... [...]]
     ...]
  note: any of the three items in a connection here can be `nil`."
  (let [keyed-resources (keyed :name resources)]
    (->>
      (for [[outdef resource-name indef] connections]
        (let [[block-out-name output-name] outdef
              [block-in-name input-name] indef
              input (get-port ports block-in-name input-name)
              output (get-port ports block-out-name output-name)
              resource (get keyed-resources resource-name)]
          (filter (partial not-any? nil?) [[output resource]
                                           [resource input]])))
      (mapcat identity)
      )))

(defn build-graph
  [ports resources connections]
  (let [port-pairer (fn [p] [p {:type (:type p)}])
        resource-pairer (fn [a] [a {:type :resource}])
        port-nodes (map port-pairer ports)
        resource-nodes (map resource-pairer resources)]
    (as-> (uber/digraph) $
          (apply uber/add-nodes-with-attrs $ port-nodes)
          (apply uber/add-nodes-with-attrs $ resource-nodes)
          (uber/add-edges* $ connections))))
