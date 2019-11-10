(ns tournament-gen.heap
  (:import (java.util PriorityQueue)))

(defn from
  "Wrap a priority queue into clojure friendly fn"
  [cmp xs]
  (let [q (PriorityQueue. (count xs) cmp)]
    (doseq [x xs]
      (.add q x))
    q))
