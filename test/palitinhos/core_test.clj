(ns palitinhos.core-test
  (:require [clojure.test :refer :all]
            [palitinhos.core :refer :all]))

(deftest cut-generation
  (testing "possible cuts for a given line number"
    (let [lines 4]
      (is (= #{1 2 3 4} (possible-cuts-for-row lines)))))
  (testing "rows in game"
    (is (= 1 (rows-in-game [4])))
    (is (= 1 (rows-in-game [2])))
    (is (= 2 (rows-in-game [2 4]))))
  (testing "no-cuts for a game"
    (is (= [0] (no-cuts [2])))
    (is (= [0] (no-cuts [1])))
    (is (= [0 0] (no-cuts [1 7]))))

  (testing "possible cuts for a game with a single line"
    (let [game [3]]
      (is (= #{[1] [2] [3]} (cuts-for-game game)))))
  (testing "possible cuts for a game with two lines"
    (let [game [2 1]]
      (is (= #{[1 0] [2 0] [0 1]} (cuts-for-game game))))))

(deftest cut-application
  (testing "apply cut for a game"
    (is (= [3 5 5] (apply-cut [3 5 7] [0 0 2])))))

(deftest game-end
  (testing "game ends when theres nothing left to cut"
    (is (= false (game-is-over? [1 0 0])))
    (is (= false (game-is-over? [1 0])))
    (is (= true (game-is-over? [0])))
    (is (= false (game-is-over? [1 1 0])))
    (is (= true (game-is-over? [0 0 0])))))

(deftest game-play
  (testing "really simple game with a really simple playout"
    (is (= [[1]] (game-playout [1])))))

(defn winning-sequence [game-sequence]
  (even? (count game-sequence)))

(defn starting-cut [game-sequence]
  (let [starting-game (first game-sequence)
        next-game (second game-sequence)]
    (vec (map - starting-game next-game))))

(defn cut-win-ratio [game]
  (let [possible-playouts (repeatedly 10000 #(game-playout game))]
    (->> possible-playouts
         (map (juxt starting-cut winning-sequence))
         (group-by first)
         (map (fn [[k v]] [k (count (filter (comp true? second) v))])))))

(defn best-cut [game]
  (first (last (sort-by second (cut-win-ratio game)))))

(deftest best-move
  (testing "finds out if we won a game give we play the first move"
    (is (= false (winning-sequence (game-playout [1]))))
    (is (= true (winning-sequence (game-playout [1 1]))))
    (is (= false (winning-sequence (game-playout [1 1 1])))))
  (testing "finds how to cut best so we maximize our winning chances"
    (is (= [0 3] (best-cut [1 3])))
    (is (= [0 3 0] (best-cut [1 2 3])))))



