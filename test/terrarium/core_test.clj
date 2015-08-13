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
    [(in  (B :X) :a (fj 1 :kg :per :s))
     (in  (B :X) :b (fj 1 :kg :per :s))
     (out (B :X) :c (fj 1 :kg :per :s))
     (out (B :X) :d (fj 1 :kg :per :s))

     (in  (B :Y) :a (fj 2 :kg :per :s))
     (in  (B :Y) :b (fj 2 :kg :per :s))
     (out (B :Y) :c (fj 2 :kg :per :s))
     (out (B :Y) :d (fj 2 :kg :per :s))

     (in (B :Z) :a (fj 3 :kg :per :s))
     (out (B :Z) :c (fj 3 :kg :per :s))]))

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
      (is (= 13 (count (uber/nodes graph))))
      (is (= 2 (count (uber/edges graph)))))

    ; TODOs
    ; - test that outputs and inputs don't show up in the wrong places
    ))


(deftest less-simple

  (let [connections (mk-connections ports resources [[[:Z :c] :r1 [:Y :a]]
                                                     [[:Y :c] :r2 [:X :a]]])
        connections-loop (mk-connections ports resources [[[:Z :c] :r1 [:Y :a]]
                                                          [[:Y :c] :r2 [:X :a]]
                                                          [[:X :c] :r3 [:Z :a]]])
        dt (fj 1 :s)
        graph (build-graph ports resources connections)
        graph-loop (build-graph ports resources connections-loop)
        accounts (keyed :name [(->Account :r1 (fj 10 :kg))
                               (->Account :r2 (fj 10 :kg))])
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
            bigger-accounts (assoc-in accounts [:r2 :amount] (fj 1000 :kg))]
        (is (=
              (calc-active-blocks graph fluxmap blocks accounts dt)
              [(:Y blockmap) (:Z blockmap)]))
        (is (=
              (calc-active-blocks graph fluxmap blocks bigger-accounts dt)
              [(:X blockmap) (:Y blockmap) (:Z blockmap)]))
        ))

    (testing "run-step"
      (let [ret (run-step graph blocks accounts dt)
            [accounts active-blocks fluxmap] ret
            blockmap (keyed :name blocks)]
        (is (= active-blocks [(:Y blockmap) (:Z blockmap)]))
        ))


    (testing "calc-active-blocks-loop"
      (let [blockmap (keyed :name blocks)
            fluxmap (calc-net-flux graph-loop blocks)
            bigger-accounts (assoc-in accounts [:r2 :amount] (fj 1000 :kg))]
        (is (=
              (calc-active-blocks graph-loop fluxmap blocks accounts dt)
              [(:Y blockmap) (:Z blockmap)]))
        (is (=
              (calc-active-blocks graph-loop fluxmap blocks bigger-accounts dt)
              [(:X blockmap) (:Y blockmap) (:Z blockmap)]))
        ))


    (testing "run-step-loop"
      (let [expected [[[10 10 10] [:X :Y :Z] [1 1 -2]]
                      [[10 10 10] [:X :Y :Z] [1 1 -2]]
                      [[10 10 10] [:X :Y :Z] [1 1 -2]]
                      [[10 10 10] [:X :Y :Z] [1 1 -2]]
                      [[10 10 10] [:X :Y :Z] [1 1 -2]]
                      [[10 10 10] [:X :Y :Z] [1 1 -2]]]
            expected (zipmap (map inc (range)) expected)
            blockmap (keyed :name blocks)]
        (doseq [[N ex] expected]
          (let [ret (run-steps N graph blocks accounts dt)]
            (trace ret)))
        #_(trace (map (fn [[k v]] [k (get-in v [:rate :v])]) fluxmap))
        #_(is (= active-blocks [(:Y blockmap) (:Z blockmap)]))
        #_(is (= 0 (get-in (trace fluxmap) [:r3 :rate :v])))))
    )
  )