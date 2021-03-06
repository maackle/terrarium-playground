(ns terrarium.repl
  (:use clojure.test
        clojure.repl
        terrarium.core)
  (:require [ubergraph.core :as uber]
            [clojure.tools.namespace.repl :refer [refresh]]
            [frinj.ops :as frinj :refer (fj fj- fj+ fj* to)]
            [terrarium.data-humans :as data]
            ))
