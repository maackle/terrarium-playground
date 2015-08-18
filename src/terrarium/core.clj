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

(defn get-resource-name [graph edge] (uber/attr graph edge :name))

(defn edge-to-map
  [edge]
  (let [f (fn [n] [(:type n) n])]
    (into {} (map f [(uber/src edge) (uber/dest edge)]))))

(defn calc-net-flux
  "Calculate net in/out flow through each Account."
  [graph active-blocks]
  (let [edges (uber/edges graph)
        active-block-set (set active-blocks)
        edgemaps (map edge-to-map edges)
        reduce-fn (fn [fluxmap edgemap]
                    (if-let [port (if (contains? edgemap :input) (:input edgemap) (:output edgemap))]
                      (if (contains? active-block-set (:block port))
                        (let [resource (:resource edgemap)
                              rate (port-flux port)
                              res-name (:name resource)]
                          (-> fluxmap
                              (update-in [res-name :rate] #(if % (fj+ % rate) rate))
                              (update-in [res-name :ports] #(conj % port))))
                        fluxmap)
                      fluxmap))]
    (reduce reduce-fn {} edgemaps)))

(defn get-flux-rates
  [fluxmap]
  (get-keys-in fluxmap [:rate :v] [:r1 :r2 :r3] 0))

(defn calc-active-blocks
  [blocks accounts fluxmap dt]
  (let [rf (fn [active-blocks {account-name :name account-amount :amount}]
             (if-let [flux (get fluxmap account-name)]
               (let [{flux-rate :rate flux-ports :ports} flux
                     flux-amount (fj* flux-rate dt)
                     inputs (filter (comp (partial = :input) :type) flux-ports)
                     input-blocks (->> inputs (map :block) set)]
                 (if (> 0 (:v (fj+ account-amount flux-amount)))
                   (remove #(contains? input-blocks %) active-blocks)
                   active-blocks))
               active-blocks))]
    (reduce rf blocks (vals accounts))))

(defn apply-flux
  [accounts fluxmap dt]
  (let [f (fn [accounts resource-name flux]
            (let [flux-amount (fj* (:rate flux) dt)]
              (update-in accounts [resource-name :amount] (partial fj+ flux-amount))))]
    (reduce-kv f accounts fluxmap)))

(defn calc-equilibrium
  [graph accounts blocks dt]
  (let [iter (fn [active-blocks]
               (let [fluxmap (calc-net-flux graph active-blocks)
                     active-blocks' (calc-active-blocks active-blocks
                                                        accounts
                                                        fluxmap
                                                        dt)]
                 (if (= active-blocks active-blocks')
                   active-blocks
                   (recur active-blocks'))
                 ))]
    (iter blocks)))

(defn run-step
  [graph accounts blocks dt]
  (let [active-blocks (calc-equilibrium graph accounts blocks dt)
        fluxmap (calc-net-flux graph active-blocks)
        accounts' (apply-flux accounts fluxmap dt)]
    [accounts' active-blocks fluxmap]))

(defn run-steps
  [N graph accounts blocks dt]
  (let [ret (run-step graph accounts blocks dt)
        [accounts' blocks' fluxmap] ret]
    (if (> N 1)
      (recur (- N 1) graph accounts' blocks dt)
      ret)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  )
