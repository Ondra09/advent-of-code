#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/2

(defn split-input [s]
  (str/split s #"\n"))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(Integer/parseInt %))
                (vec)))
