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

(defn calc-net-flow
  "Calculate net in/out flow through each Account"
  [graph dt]
  (let [edges (uber/edges graph)
        get-amount #(if % (fj* % dt))
        reduce-fn (fn [flux edge]
                    (let [edge-key (get-resource-name graph edge)
                          port-amounts (map
                                         (comp get-amount port-flux)
                                         [(uber/src edge) (uber/dest edge)])
                          update-fn (fn [v]
                                      (let [amounts (filter identity (conj port-amounts v))
                                            total (reduce fj+ amounts)]
                                        total))]
                      (update flux edge-key update-fn)))]
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
