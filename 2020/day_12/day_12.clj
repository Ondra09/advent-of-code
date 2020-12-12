#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ instruction num] (re-matches #"(\w)(\d+)" line)]
    [(keyword instruction) (read-string num)]))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))

;; --------> X
;; |
;; |
;; |
;; v
;; Y

(def ship-state {:facing [1 0] :pos [0 0]})
(def ship-waypoint-state {:facing [10 1] :pos [0 0]})



(defn abs [n] (max n (- n)))
(defn manhattan-distance [[x y]]
  (+ (abs x) (abs y)))

(defn rotate-left [[x y]]
  [(unchecked-negate y) x])

(defn rotate-right [[x y]]
  [y (unchecked-negate x)])

(defn rotate-n [fun [x y] amount]
  (if (= 0 amount)
    [x y]
    (recur fun (fun [x y]) (- amount 90))
    ))

(defn rotate-left-n [[x y] amount]
  (rotate-n rotate-left [x y] amount))

(defn rotate-right-n [[x y] amount]
  (rotate-n rotate-right [x y] amount))

(defn move-ship [state [inst val]]
  (cond (= inst :N) (update state :pos (partial mapv + [0 val]))
        (= inst :S) (update state :pos (partial mapv + [0 (unchecked-negate val)]))
        (= inst :E) (update state :pos (partial mapv + [val 0]))
        (= inst :W) (update state :pos (partial mapv + [(unchecked-negate val) 0]))
        (= inst :L) (update state :facing #(rotate-left-n % val))
        (= inst :R) (update state :facing #(rotate-right-n % val))
        (= inst :F) (update state :pos #(mapv + % (mapv * (state :facing) [val val])))
        ))


(defn move-ship-and-waypoint [state [inst val]]
    (cond (= inst :N) (update state :facing (partial mapv + [0 val]))
          (= inst :S) (update state :facing (partial mapv + [0 (unchecked-negate val)]))
          (= inst :E) (update state :facing (partial mapv + [val 0]))
          (= inst :W) (update state :facing (partial mapv + [(unchecked-negate val) 0]))
          (= inst :L) (update state :facing #(rotate-left-n % val))
          (= inst :R) (update state :facing #(rotate-right-n % val))
          (= inst :F) (update state :pos #(mapv + % (mapv * (state :facing) [val val])))
        ))

(def part-1 (reduce (fn [accum [inst val]] (move-ship accum [inst val])) ship-state input))
(def part-2 (reduce (fn [accum [inst val]] (move-ship-and-waypoint accum [inst val])) ship-waypoint-state input))

(println "Part 1: " (manhattan-distance (part-1 :pos)))
(println "Part 2: " (manhattan-distance (part-2 :pos)))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
