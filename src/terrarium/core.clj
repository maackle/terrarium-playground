(ns terrarium.core
  (:use clojure.pprint)
  (:require [ubergraph.core :as uber]
            [frinj.core :refer (zero)]
            [frinj.ops :as frinj :refer (fj fj- fj+ fj* fj< to)]
            [terrarium.model :refer :all]
            [terrarium.util :refer :all]
            )
  (:gen-class))

(defn fj-inv
  "Unary negation for frinj values (there must be a better way!)"
  [q]
  (fj- q q q))

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

(defn calc-net-flux
  "Calculate net in/out flow through each Account."
  [graph dt]
  (let [edges (uber/edges graph)
        edgemaps (map edge-to-map edges)
        get-amount #(if % (fj* % dt))
        initial-fluxmap {:amount nil :ports [] :blocks #{}}
        reduce-fn (fn [fluxmap edgemap]
                    (if-let [port (if (contains? edgemap :input) (:input edgemap) (:output edgemap))]
                      (let [resource (:resource edgemap)
                            flux (port-flux port)
                            amount (fj* flux dt)
                            res-name (:name resource)]
                        (-> fluxmap
                          (update-in [res-name :amount] #(if % (fj+ % amount) amount))
                          (update-in [res-name :ports] #(conj % port))))
                      fluxmap))]
    (reduce reduce-fn initial-fluxmap edgemaps)))

(defn calc-active-blocks
  [{:keys [graph accounts]} dt]
  (let [blocks (->> graph uber/nodes (filter port-node?) (map :block) distinct)
        fluxmap (calc-net-flux graph dt)
        rf (fn [active-blocks account]
             (let [{account-name :name account-amount :amount} account
                   {flux-amount :amount flux-ports :ports} (get fluxmap account-name)
                   inputs (filter (comp (partial = :input) :type) flux-ports)
                   input-blocks (->> inputs (map :block) set)]
               (if (> 0 (:v (fj+ account-amount flux-amount)))
                 (remove #(contains? input-blocks %) active-blocks)
                 active-blocks)
               ))]
    (reduce rf blocks accounts)))

(defn do-step
  [state dt]
  (let [graph (:graph @state)
        accounts (:accounts @state)]

    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
