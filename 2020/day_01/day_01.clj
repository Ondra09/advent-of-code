#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/1

(defn split-input [s]
  (str/split s #"\n"))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                ;;(map #(read-string %))
                (map #(Integer/parseInt %))
                (vec)))

(defn find-twos [input]
  (for [x input
        y input
        :when (= (+ x y) 2020)]
    [x y]
    ))

;; result is twice in result set, just take first one
(def part-1-parts (first(find-twos input)))

(def result-1 (* (first part-1-parts) (second part-1-parts)))

(println "Part 1 result is: " result-1)

;;;;;;;;;;;;;;;;;;;;;;;;; Part 2
(defn find-three-times [input]
  (for [x input
        y input
        z input
        :when (= (+ x y z) 2020)]
    [x y z]))

(def part-2-parts (first (find-three-times input)))

(println "Part 2 result is: " (apply * part-2-parts))
