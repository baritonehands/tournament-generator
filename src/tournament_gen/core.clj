(ns tournament-gen.core
  (:require [clojure.set :as set]))

(defn rotate [n ps]
  (concat (drop n ps) (take n ps)))

(defn rotate-pivot [n [p & ps]]
  (cons p (rotate n ps)))

(defn round [size ps]
  (apply mapcat vector (partition (Math/floorDiv (count ps) size) ps)))

(defn rounds [size n ps]
  (take n (iterate #(rotate 2 (round size %)) ps)))

(defn rounds-plus [size n ps]
  (take n (cons ps
                (iterate #(rotate 2 (round size %)) (round size ps)))))

(defn set->map [gset]
  (into {}
        (for [k gset]
          [k gset])))

(defn verify [size gs]
  (apply merge-with
         set/union
         (for [game (mapcat (partial partition size) gs)]
           (set->map (set game)))))

(defn vert [vs cols]
  (->> vs
       (partition cols)
       (map vec)
       (vec)))

(def pairs [[0 0] [1 1] [0 2] [2 2]
            [1 0] [3 0] [2 1] [3 2]
            [2 0] [0 1] [3 1] [1 2]])

(defn match-up [vs]
  (for [pair pairs]
    (get-in vs pair)))

(defn rounds-diag [size n ps]
  (take n (iterate #(rotate 2 (match-up (vert % (Math/floorDiv (count ps) size)))) ps)))

(defn empty-matched [ps]
  (into {} (for [p ps] [p #{p}])))

(defn merge-placed [idx placed ps]
  (let [pset (set (cons idx ps))]
    (->> (filter pset (keys placed))
         (reduce #(update %1 %2 set/union pset) placed))))

(defn round-matched [size ps matched]
  (loop [idx 0
         pset (set/difference (set ps) (matched idx))
         placed matched
         round (empty-matched ps)]
    (println "\nidx" idx)
    (println "pset" pset)
    (println "placed" placed)
    (println "round" round)
    (cond
      (= idx (count ps)) (distinct (vals round))
      (< (count (round idx)) size) (let [opts (map placed pset)
                                         to-place (take (dec size) pset)
                                         new-placed (merge-placed idx placed to-place)]
                                     (println "opts" opts)
                                     (println "to-place" to-place)
                                     (println "new-placed" new-placed)
                                     (recur (inc idx)
                                            (set/difference (set ps) (new-placed (inc idx)))
                                            new-placed
                                            (merge-placed idx round to-place)))
      :else (recur (inc idx)
                   (set/difference (set ps) (placed (inc idx)))
                   placed
                   round))))

(defn rounds-tracked [size n ps]
  (loop [idx n
         matched (empty-matched ps)
         rounds []]
    (if (zero? idx)
      rounds
      (let [round (round-matched size ps matched)]
        (recur
          (dec idx)
          (merge-with set/union matched (verify size [round]))
          (conj rounds round))))))
