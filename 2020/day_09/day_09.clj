#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/9

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (read-string line))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (vec)))

(defn find-twos [input result]
  (for [[idx x] (map-indexed vector input)
        idx-y (range (inc idx))
        :let [y (nth input idx-y)]
        :when (= (+ x y) result)]
    [x y]))

(defn check-sequence [coll prefix shift]
  (not (empty? (find-twos (take prefix (drop shift coll)) (nth coll (+ prefix shift))))))

(defn find-invalid [coll prefix]
  (nth coll
       (+ prefix (first (filter #(not(check-sequence coll prefix %)) (range (- (count input) prefix)))))))

(defn sum-numbers-up-to [coll max]
  (reduce (fn [[sum idx] num] (if (>= sum max) (reduced [sum idx]) [(+ sum num) (inc idx)])) [0 0] coll))

(defn find-subset-equal-to-sum [coll sum]
  (reduce (fn [[beg end] idx]
            (let [[sum-coll length] (sum-numbers-up-to (drop idx coll) sum)]
              (if (= sum sum-coll)
                (reduced [idx length])
                [0 0]
                )))
          [0 0] (range (count coll))))


(def part-1 (find-invalid input 25))
(println "Part 1 result: " part-1)

(def subset (find-subset-equal-to-sum input part-1))

(def result-range (take (second subset) (drop (first subset) input)))
(def minr (apply min result-range))
(def maxr (apply max result-range))

(println "Part 2 result: " (+ minr maxr))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
