#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/22

(defn split-input [s]
  (str/split s #"\n\n"))

(defn parse-input [s]
  (->> (rest (str/split s #"\n"))
       (map read-string)
       ))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))))

(defn is-player-1-round-winner? [deck1 deck2]
  (cond (empty? deck1) false
        (empty? deck2) true
        (> (first deck1) (first deck2)) true))

(defn play-game [[deck1 deck2] recur]
  (loop [deck1 deck1
         deck2 deck2
         rounds1 []
         rounds2 []]
    (cond (or (empty? deck1)
              (empty? deck2)) [deck1 deck2]
          (or (some #(= %1 deck1) rounds1)
              (some #(= %1 deck2) rounds2)) [deck1 '()] ;; player 1 is winner
          :else
          (let [top1 (first deck1)
                top2 (first deck2)
                ;;a (println " " top1 " " top2 " " deck1 " " deck2)
                [d1 d2] (if (and recur
                                 (<= top1 (count (rest deck1)))
                                 (<= top2 (count (rest deck2))))
                          (do
                            ;;(println "subgame")
                            (play-game [(take top1 (rest deck1)) (take top2 (rest deck2))] recur))
                          [deck1 deck2])]
            (if (is-player-1-round-winner? d1 d2)
              (recur (concat (rest deck1) (list top1 top2)) (rest deck2) (conj rounds1 deck1) (conj rounds2 deck2))
              (recur (rest deck1) (concat (rest deck2) (list top2 top1)) (conj rounds1 deck1) (conj rounds2 deck2))
              ))
          )))

(defn play-game-1 [input]
  (play-game input false))

(defn compute-result [[deck1 deck2]]
  (+ (second (reduce (fn [[idx acc] val] [(inc idx) (+ acc (* val idx))]) [1 0] (reverse deck1)))
     (second (reduce (fn [[idx acc] val] [(inc idx) (+ acc (* val idx))]) [1 0] (reverse deck2)))))

(defn play-game-2 [[deck1 deck2]]
  (play-game input true))

(println "Part 1 result: " (compute-result (play-game-1 input)))
(println "Part 2 result: " (compute-result (play-game-2 input)))
