(ns terrarium.core-test
  (:use clojure.test)
  (:require [frinj.ops :refer (fj)]
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

(def mk-connection (connection-fn ports))

(deftest simple

  (def connections
    [(mk-connection [:X :a] [:Y :c] :r1)])

  (def graph (build-graph ports connections))

  (testing "single connection"
    (def flux (calc-net-flow graph (fj 1 :day)))
    (is (= 10 flux))))

(deftest less-simple

  (def connections
    [(mk-connection [:Z :a] [:Y :b] :r1)])

  (def graph (build-graph ports connections))

  (testing "2"
    (def flux (calc-net-flow graph (fj 1 :day)))
    (is (= 17/2500 (get-in flux [:r1 :v])))))
