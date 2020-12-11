#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (read-string line))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                ((fn [coll] (conj coll 0)))
                (sort)
                (vec)))


(def difs (map (fn [[x1 x2]] (- x2 x1)) (partition 2 1 input)))

(def result-1 (frequencies difs))

(println "Part 1 result: " (* (result-1 1) (inc (result-1 3)))  )

(println "Part 2 result: " )
(println input)
(println difs)
(apply * (map (fn [[x1 x2]] (cond (= x1 1 x2) 2
                         :else 1)) (partition 2 1 difs)))
;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
;; 1 1 1 1
;; 1 1
;; 1 1 1 1
;; 1
;; 1 1
;; 1 1 1 1
;; 1 1 1 1
;; 1 1 1
;; 1 1
;; 1 1 1
;; 1 1 1 1
;; 1 1 1 1
;; 1 1 1
;; 1 1 1
;; 1 1 1
;; 1 1 1
;; 1 1 1
;; 1 1 1 1
;; 1 1 1
;; 1 1 1
;; 1 1 1 1
;; 1 1 1

(println (* 7 2 7 1 2 7 7 4 2 4 7 7 4 4 4 4 4 7 4 4 7 4))

