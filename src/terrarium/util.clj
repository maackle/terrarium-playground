(ns terrarium.util)

(defn keyed
  "Turn a seq of maps into a map of maps, where the key of the new map equals the value at key"
  [k vs]
  (reduce (fn [a v] (assoc a (k v) v)) {} vs))


(defn by-name [coll name]
  "Get first item in sequence whose :name property equals `name`"
  (first (filter #(= name (get % :name)) coll)))
