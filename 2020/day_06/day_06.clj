#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/2

(defn split-input [s]
  (str/split s #"\n\n"))

(defn parse-input [line]
  (map #(set %) (str/split line #"\n")))

(defn modify-input [col]
  col)

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (map #(modify-input %))))

(defn sum-result [input fun]
  (apply + (map #(count (apply fun %)) input)))

(println "Part 1 result: " (sum-result input clojure.set/union))
(println "Part 2 result: " (sum-result input clojure.set/intersection))
