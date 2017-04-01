(ns tournament-gen.core)

(defn rotate [n ps]
  (concat (drop n ps) (take n ps)))

(defn round [size ps]
  (apply mapcat vector (partition (Math/floorDiv (count ps) size) ps)))

(defn rounds [size n ps]
  (take n (iterate #(round size (rotate 2 %)) ps)))

(defn set->map [gset]
  (into {}
        (for [k gset]
          [k gset])))

(defn verify [size gs]
  (apply merge-with
         clojure.set/union
         (for [game (mapcat (partial partition size) gs)]
           (set->map (set game)))))
