(ns terrarium.core
  (:require [ubergraph.core :as uber])
  (:require [frinj.core :refer (zero)])
  (:require [frinj.ops :as frinj :refer (fj fj- fj+ fj* to)])
  (:require [terrarium.util :refer (keyed)])
  (:gen-class))

(defn fj-inv
  "Unary negation for frinj values (there must be a better way!)"
  [q]
  (fj- q (fj* q 2)))

(defn port-flux
  "Get positive flow for outputs, negative flow for inputs"
  [port]
  (case (:type port)
    :output (:rate port)
    :input (fj-inv (:rate port))
    nil))

#_(defn get-resources
  [graph]
  (let [edges (uber/edges graph)]
    (distinct (map #(uber/attrs graph %) edges))))

#_(defn get-ports
  [graph]
  (let [edges (uber/edges graph)]
    (mapcat #(conj [] (uber/src %1) (uber/dest %1)) edges)
))

(defn get-resource-name [graph edge] (uber/attr graph edge :name))

(defn edge-to-map
  [edge]
  (let [f (fn [n] [(:type n) n])]
    (into {} (map f [(uber/src edge) (uber/dest edge)]))))

(defn edge-reduce-fn
  [fluxmap edge]
  (let [m (edge-to-map edge)
        resource (:resource m)
        port (if (contains? m :input) (:input m) (:output m))
        flux (port-flux port)
        update-fn (partial fj+ flux)]
    (update m (:name resource) update-fn)))

(defn calc-net-flow
  "Calculate net in/out flow through each Account."
  [graph dt]
  (let [edges (uber/edges graph)
        get-amount #(if % (fj* % dt))
        reduce-fn (fn [fluxmap edge]
                    (let [m (edge-to-map edge)]
                      (if-let [port (if (contains? m :input) (:input m) (:output m))]
                        (let [resource (:resource m)
                              flux (port-flux port)
                              amount (fj* flux dt)
                              update-fn #(if % (fj+ % amount) amount)]
                          (update fluxmap (:name resource) update-fn))
                        fluxmap)))]
    (reduce reduce-fn {} edges)))


(defn do-step
  [state dt]
  (let [graph (:graph @state)
        accounts (:accounts @state)]
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
