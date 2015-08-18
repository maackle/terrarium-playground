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
        graph-linear (build-graph ports resources connections)]

    (testing "edge-to-map"
      (let [maps (map edge-to-map (uber/edges graph-linear))]
        (is (= 2 (count maps)))
        (is (every? #(contains? % :resource) maps))
        ))))

(deftest graph-structure

  (let [connections (mk-connections ports resources [[[:X :a] :r1 [:Y :c]]])
        graph-linear (build-graph ports resources connections)]

    (testing "graph structure"
      (is (= 13 (count (uber/nodes graph-linear))))
      (is (= 2 (count (uber/edges graph-linear)))))

    ; TODOs
    ; - test that outputs and inputs don't show up in the wrong places
    ))

(deftest less-simple

  (let [connections-loop (mk-connections ports resources [[[:Y :c] :r1 [:X :a]]
                                                          [[:Z :c] :r2 [:Y :a]]
                                                          [[:X :c] :r3 [:Z :a]]])
        dt (fj 1 :s)
        graph-loop (build-graph ports resources connections-loop)
        accounts (keyed :name [(->Account :r1 (fj 10 :kg))
                               (->Account :r2 (fj 10 :kg))
                               (->Account :r3 (fj 10 :kg))])
        state (atom {:graph graph-loop
                     :accounts accounts})]

    (testing "calc-net-flux"
      (let [expected [[[]         [0 0 0]]
                      [[:X]       [-1 0 1]]
                      [[:Y]       [2 -2 0]]
                      [[:X :Y]    [1 -2 1]]
                      [[:Z]       [0 3 -3]]
                      [[:Z :X]    [-1 3 -2]]
                      [[:Z :Y]    [2 1 -3]]
                      [[:Z :Y :X] [1 1 -2]]]
            blockmap (keyed :name blocks)]
        (doseq [[block-names rates] expected]
          (let [fluxmap (calc-net-flux graph-loop (get-keys blockmap block-names))
                expected-rates (get-keys-in fluxmap [:rate :v] [:r1 :r2 :r3] 0)]
            (is (= rates expected-rates))))
        ))

    (testing "apply-flux"
      (let [fluxmap (calc-net-flux graph-loop blocks)
            accounts' (apply-flux accounts fluxmap dt)]
        (is ((comp not =) accounts accounts'))))

    (testing "calc-active-blocks"
      (let [blockmap (keyed :name blocks)
            fluxmap (calc-net-flux graph-loop blocks)
            smaller-accounts (assoc-in accounts [:r3 :amount] (fj 0 :kg))
            bigger-accounts (assoc-in accounts [:r2 :amount] (fj 1000 :kg))]
        (is (=
              (calc-active-blocks blocks smaller-accounts fluxmap dt)
              [(:X blockmap) (:Y blockmap)]))
        (is (=
              (calc-active-blocks blocks accounts fluxmap dt)
              [(:X blockmap) (:Y blockmap) (:Z blockmap)]))
        ))

    (testing "run-step-loop"
      (let [expected [[[11 11 8] [:X :Y :Z] [1 1 -2]]
                      [[12 12 6] [:X :Y :Z] [1 1 -2]]
                      [[13 13 4] [:X :Y :Z] [1 1 -2]]
                      [[14 14 2] [:X :Y :Z] [1 1 -2]]
                      [[15 15 0] [:X :Y :Z] [1 1 -2]]
                      [[16 13 1] [:X :Y] [1 -2 1]]
                      [[17 11 2] [:X :Y] [1 -2 1]]
                      [[18 12 0] [:X :Y :Z] [1 1 -2]]
                      [[19 10 1] [:X :Y] [1 -2 1]]
                      [[20 8 2] [:X :Y] [1 -2 1]]
                      [[21 9 0] [:X :Y :Z] [1 1 -2]]
                      [[22 7 1] [:X :Y] [1 -2 1]]
                      [[23 5 2] [:X :Y] [1 -2 1]]
                      [[24 6 0] [:X :Y :Z] [1 1 -2]]
                      [[25 4 1] [:X :Y] [1 -2 1]]
                      [[26 2 2] [:X :Y] [1 -2 1]]
                      [[27 3 0] [:X :Y :Z] [1 1 -2]]
                      [[28 1 1] [:X :Y] [1 -2 1]]
                      [[27 1 2] [:X] [-1 0 1]]
                      [[28 2 0] [:X :Y :Z] [1 1 -2]]
                      [[29 0 1] [:X :Y] [1 -2 1]]
                      [[28 0 2] [:X] [-1 0 1]]
                      [[29 1 0] [:X :Y :Z] [1 1 -2]]
                      [[28 1 1] [:X] [-1 0 1]]
                      [[27 1 2] [:X] [-1 0 1]]
                      [[28 2 0] [:X :Y :Z] [1 1 -2]]
                      [[29 0 1] [:X :Y] [1 -2 1]]]
            expected (map list (map inc (range)) expected)
            blockmap (keyed :name blocks)]
        (doseq [[N [ex-amounts ex-blocks ex-flux]] expected]
          (let [ret (run-steps N graph-loop accounts blocks dt)
                [accounts active-blocks fluxmap] ret
                account-amounts (get-keys-in accounts [:amount :v] [:r1 :r2 :r3])
                block-names (map :name active-blocks)
                flux-rates (get-keys-in fluxmap [:rate :v] [:r1 :r2 :r3] 0)]
            (is (= ex-amounts account-amounts))
            (is (= ex-blocks block-names))
            (is (= ex-flux flux-rates))
            ; (prn N account-amounts (sort block-names) flux-rates)
            ))
        #_(trace (map (fn [[k v]] [k (get-in v [:rate :v])]) fluxmap))
        #_(is (= active-blocks [(:Y blockmap) (:Z blockmap)]))
        #_(is (= 0 (get-in (trace fluxmap) [:r3 :rate :v])))))
    )
  )