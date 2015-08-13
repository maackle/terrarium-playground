(ns terrarium.core-test
  (:use clojure.test
        clojure.pprint)
  (:require [frinj.ops :refer (fj)]
            [ubergraph.core :as uber]
            [terrarium.model :refer :all]
            [terrarium.core :refer :all]
            [terrarium.util :refer :all]
            ))


(def blocks (map ->Block [:X :Y :Z]))

(def ports
  (let [B (partial get (keyed :name blocks))
        in (partial ->Port :input)
        out (partial ->Port :output)]
    [(in  (B :X) :a (fj 20 :L :per :day))
     (in  (B :X) :b (fj 2 :kg :per :day))
     (out (B :X) :c (fj 3 :lb :per :day))
     (out (B :X) :d (fj 4 :lb :per :day))

     (in  (B :Y) :a (fj 5 :L :per :day))
     (in  (B :Y) :b (fj 5 :L :per :day))
     (out (B :Y) :c (fj 6 :L :per :day))
     (out (B :Y) :d (fj 700 :g :per :day))

     (out (B :Z) :a (fj 800 :mL :per :day))]))

(def resources (map mk-resource [:r1 :r2 :r3]))

(deftest core

  (let [connections (mk-connections ports resources [[[:X :a] :r1 [:Y :c]]])
        graph (build-graph ports resources connections)]

    (testing "edge-to-map"
      (let [maps (map edge-to-map (uber/edges graph))]
        (is (= 2 (count maps)))
        (is (every? #(contains? % :resource) maps))
        ))))

(deftest graph-structure

  (let [connections (mk-connections ports resources [[[:X :a] :r1 [:Y :c]]])
        graph (build-graph ports resources connections)]

    (testing "graph structure"
      (is (= 12 (count (uber/nodes graph))))
      (is (= 2 (count (uber/edges graph)))))

    ; TODOs
    ; - test that outputs and inputs don't show up in the wrong places
    ))


(deftest less-simple

  (let [connections (mk-connections ports resources [[[:Z :a] :r1 [:Y :b]]
                                                     [[:Y :c] :r2 [:X :a]]])
        dt (fj 1 :day)
        graph (build-graph ports resources connections)
        accounts (keyed :name [(->Account :r1 (fj 10 :L))
                               (->Account :r2 (fj 10 :L))])
        state (atom {:graph graph
                     :accounts accounts})]

    (testing "calc-net-flux"
      (def fluxmap (calc-net-flux graph blocks))
      (is (> 0 (get-in fluxmap [:r1 :rate :v])))
      #_(is (= :umm (get-in fluxmap [:r1 :ports]))))

    (testing "calc-net-flux-zero"
      (def fluxmap (calc-net-flux graph []))
      (is (= nil (get-in fluxmap [:r1 :amount :v])))
      (is (= nil (get-in fluxmap [:r1 :ports]))))

    (testing "apply-flux"
      (let [fluxmap (calc-net-flux graph blocks)
            accounts' (apply-flux fluxmap accounts dt)]
        (is ((comp not =) accounts accounts'))))

    (testing "calc-active-blocks"
      (let [blockmap (keyed :name blocks)
            fluxmap (calc-net-flux graph blocks)
            bigger-accounts (assoc-in accounts [:r2 :amount] (fj 1000 :L))]
        (is (=
              (calc-active-blocks graph fluxmap blocks accounts dt)
              [(:Y blockmap) (:Z blockmap)]))
        (is (=
              (calc-active-blocks graph fluxmap blocks bigger-accounts dt)
              [(:X blockmap) (:Y blockmap) (:Z blockmap)]))
        ))

    (testing "do-step"
      (let [ret (do-step graph blocks accounts dt)
            [accounts active-blocks fluxmap] ret
            blockmap (keyed :name blocks)]
        (is (= active-blocks [(:Y blockmap) (:Z blockmap)]))
        (is (= 0 (get-in fluxmap [:r1 :rate])))))
    )
  )