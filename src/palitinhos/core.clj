(ns palitinhos.core)

;; cut genaration

(defn possible-cuts-for-row
  [lines]
  (set (range 1 (inc lines))))

(defn rows-in-game [game]
  (count game))

(defn no-cuts [game]
  (vec (replicate (rows-in-game game) 0)))

(defn cuts-for-game
  [game]
  (let [zero-cuts (no-cuts game)]
    (set
      (for [game-row (range (count game))
            possible-cuts (possible-cuts-for-row (nth game game-row))]
        (assoc zero-cuts game-row possible-cuts)))))

;; cut application

(defn apply-cut [game cut]
  (vec (map - game cut)))

;; game end
(defn game-is-over? [game]
  (= 0 (reduce + game)))

;; game-play
(defn randomly-cut [game]
  (let [possible-cuts (cuts-for-game game)
        random-cut (-> possible-cuts shuffle first)]
    (apply-cut game random-cut)))

(defn game-playout [game]
  (take-while (comp not game-is-over?)
              (iterate randomly-cut game)))

;; find best move

