(ns terrarium.core-test
  (:use clojure.test
        clojure.pprint)
  (:require [frinj.ops :refer (fj)]
            [ubergraph.core :as uber]
            [terrarium.model :refer :all]
            [terrarium.core :refer :all]
            [terrarium.util :refer (keyed)]))


(def blocks (map ->Block [:X :Y :Z]))

(def ports
  (let [B (partial get (keyed :name blocks))
        in (partial ->Port :input)
        out (partial ->Port :output)]
    [(in  (B :X) :a (fj 1 :kg :per :day))
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

  (def connections (mk-connections ports resources [[[:X :a] :r1 [:Y :c]]]))

  (def graph (build-graph ports resources connections))

  (testing "edge-to-map"
    (let [maps (map edge-to-map (uber/edges graph))]
      (is (= 2 (count maps)))
      (is (every? #(contains? % :resource) maps))
		)))

(deftest simple

  (def connections (mk-connections ports resources [[[:X :a] :r1 [:Y :c]]]))

  (def graph (build-graph ports resources connections))

  (testing "graph structure"
    (is (= 12 (count (uber/nodes graph))))
    (is (= 2 (count (uber/edges graph))))))


#_(deftest less-simple

  (def connections (mk-connections ports resources
                                   [[:Z :a] :r1 [:Y :b]]))

  (def graph (build-graph ports resources connections))

  (testing "2"
    (def flux (calc-net-flow graph (fj 1 :day)))
    (prn "flux" flux)
    (is (= 17/2500 (get-in flux [:r1 :v])))))
