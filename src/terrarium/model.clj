(ns terrarium.model
  (:require [ubergraph.core :as uber])
  (:require [frinj.jvm :refer (frinj-init!)])
  (:require [terrarium.util :refer (keyed)]))

(frinj-init!)

(defrecord Block [name])
(defrecord Port [type block desc rate])
(defrecord Resource [name])

(defn get-port
  ([ports block-name port-name]
   (->> ports
        (filter #(and (= block-name (get-in % [:block :name])) (= port-name (:desc %))))
        (first))))

(defn connection-fn [ports]
  (fn [[block-out-name output-name] [block-in-name input-name] resource-name]
    (let [input (get-port ports block-in-name input-name)
          output (get-port ports block-out-name output-name)
          data (->Resource resource-name)]
      [output input data])))
