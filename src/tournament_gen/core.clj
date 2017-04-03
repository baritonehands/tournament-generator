(ns tournament-gen.core)

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
         clojure.set/union
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
