(ns terrarium.data-humans
  (:require [ubergraph.core :as uber]
            [clojure.pprint :refer (pprint)]
            [frinj.ops :refer (fj to)]
            [terrarium.model :refer :all]
            [terrarium.core :refer :all]
            [terrarium.util :refer :all])
  )

(def blocks (map ->Block [:human :plants :compost-bin]))

(def ports
  (let [B (partial get (keyed :name blocks))
        in (partial ->Port :input)
        out (partial ->Port :output)]
    [(in  (B :human) :water (fj 2 :L :per :day))
     (in  (B :human) :food (fj 2000 :calories :per :day))
     (out (B :human) :urine (fj 1.8 :kg :per :day))
     ; (out (B :human) :urine (fj 1.8 :L :per :day))
     (out (B :human) :feces (fj 500 :g :per :day))

     (in  (B :plants) :compost (fj 360 :lb :per :year))
     (in  (B :plants) :water (fj 360 :L :per :year))
     (out (B :plants) :vegetables (fj 800 :kcal :per :year))
     (out (B :plants) :biomass (fj 200 :lb :per :year))

     (in (B :compost-bin) :waste (fj 360 :lb :per :year))
     (out (B :compost-bin) :compost (fj 300 :lb :per :year))]))

(def resources (map mk-resource [:water
                                 :food
                                 :waste
                                 :compost
                                 ]))

(def connections
  (mk-connections ports resources
                  [[[:compost-bin :compost] :compost [:plants :compost]]
                   [nil :water [:plants :water]]

                   [[:plants :vegetables] :food [:human :food]]
                   [nil :water [:human :water]]

                   [[:plants :biomass] :waste [:compost-bin :waste]]
                   [[:human :urine] :waste [:compost-bin :waste]]
                   [[:human :feces] :waste [:compost-bin :waste]]

                   ]))

(def graph (build-graph ports resources connections))

(def accounts (keyed :name [(mk-account :water 1000 :L)
                            (mk-account :food 10 :kcal)
                            (mk-account :waste 0 :kg)
                            (mk-account :compost 100 :kg)]))

(defn debug-step
  [ret]
  (let [[accounts active-blocks fluxmap] ret
        account-names [:food :water :compost :waste]
        account-amounts (get-keys-in accounts [:amount] account-names)
        block-names (sort (map :name active-blocks))
        flux-rates (get-keys-in fluxmap [:rate :v] account-names 0)
        fmt-account-values (map (fn [acct amt]
                                  (:v (to-account-units amt acct)))
                                (get-keys accounts account-names)
                                account-amounts)]
    [(zipmap account-names fmt-account-values) (sort block-names)]
    ))

(defn showme [num]
  (for [N (map inc (range num))]
    [N (debug-step (run-steps N graph accounts blocks (fj 1 :day)))]))