(ns terrarium.util)

(defn keyed
  "Turn a seq of maps into a map of maps, where the key of the new map equals the value at key"
  [k vs]
  (reduce (fn [a v] (assoc a (k v) v)) {} vs))


(defn by-name [coll name]
  "Get first item in sequence whose :name property equals `name`"
  (first (filter #(= name (get % :name)) coll)))


(defmacro and-let
  "from http://edtsech.github.io/2012/12/and-let.html"
  [bindings expr]
  (if (seq bindings)
    `(if-let [~(first bindings) ~(second bindings)]
       (and-let ~(drop 2 bindings) ~expr))
     expr))


(defn trace [x] (println "   vvvvvv   \n" x) x)