(ns tournament-gen.core
  (:require [clojure.set :as set]
            [clojure.pprint]
            [tournament-gen.heap :as heap])
  (:gen-class))

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
  (->> (for [game (mapcat (partial partition size) gs)]
         (set->map (set game)))
       (apply merge-with clojure.set/union)
       (map #(vector (first %) (count (second %))))
       (sort)))

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

(defn assign-unplayed
  "Original attempt, grab unplayed first, then anyone available"
  [p avail played size]
  (let [unplayed (set/difference avail played)
        avail-opp (set/intersection avail played)]
    (->> (concat unplayed avail-opp)
         (take (dec size))
         (cons p)
         (set))))

(defn assign-unplayed2
  "Second attempt, try to find unplayed opponent, tie goes to one who played fewer games"
  [p avail history size]
  (let [optimal (->> (for [[opp played] history
                           :when (and (contains? avail opp)
                                      (not= p opp))]
                       [[(contains? played p) (count played)] opp])
                     (sort))]
    (->> (map second optimal)
         (take (dec size))
         (cons p)
         (set))))

(defn assign-unplayed3
  "Third attempt, try to find opponents who have also not played each other"
  [p avail history size]
  (let [optimal (fn [taking]
                  (->> (for [[opp played] history
                             :when (and (contains? avail opp)
                                        (not (contains? taking opp)))]
                         [[(contains? played p) (count played)] opp])
                       (sort)
                       (first)
                       (second)))]
    (->> (iterate
           (fn [taking]
             (conj taking (optimal taking)))
           #{p})
         (drop (dec size))
         (first))))

(defn play
  "Play a single game and record in the history"
  [game history]
  (let [cmp (.comparator history)]
    (->> (for [[to-update played :as orig] history]
           (if (game to-update)
             [to-update (set/union played game)]
             orig))
         (heap/from cmp))))

(defn greedy-round
  "Play a single round and record in the history"
  [size {:keys [history rounds]}]
  (let [all (->> (map first history)
                 (set))]
    (loop [avail all
           history history
           games []]
      (if (>= (count avail) size)
        (let [p (ffirst history) ; Player with fewest opponents
              game (assign-unplayed3 p avail history size)]
          (recur
            (set/difference avail game)
            (play game history)
            (conj games game)))
        {:history history
         :rounds  (conj rounds games)}))))

(defn val-count [x y]
  (< (count (second x)) (count (second y))))

(defn empty-history [ps]
  (->> (map #(vector % #{%}) ps)
       (heap/from (comparator val-count))))

(defn history-incomplete? [cnt history]
  (some
    #(< (count %) cnt)
    (map second history)))

(defn rounds-greedy
  "Basic idea is try to assign players each round based on the single lowest
   player's opponent count. A min heap is used to pick the player who has played
   the fewest opponents."
  [size ps]
  (let [history (empty-history ps)]
    (loop [{:keys [history rounds] :as result}
           {:history history
            :rounds  []}]
      (if (history-incomplete? (count ps) history)
        (recur (greedy-round size result))
        result))))

(defn -main [& _]
  (let [ps (range 0 12)
        baseline (rounds-greedy 4 ps)
        cnt (count (:rounds baseline))]
    (println "Results for 12 total players")
    (doseq [[name gs] [["Greedy" (:rounds baseline)]
                       ["Naive" (rounds 4 cnt ps)]
                       ["Improved" (rounds-plus 4 cnt ps)]
                       ["Diagonal" (rounds-diag 4 cnt ps)]]]
      (println "\n" name)
      (clojure.pprint/pprint
        (if (> (count (first gs)) 4)
          [(mapv #(vec (partition 4 %)) gs)
           (verify 4 gs)]
          gs)))))
