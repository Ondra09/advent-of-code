#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/11

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (vec (char-array line)))

(def input (->> (slurp "input-test")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))


;; ----> Y
;; |
;; |
;; v
;; X

(def eight-neigbour [[-1 -1] [0 -1] [1 -1]
                     [-1 0] [1 0]
                     [-1 1] [0 1] [1 1]])

(defn count-neighbors [coll x y value]
  (reduce (fn [acc [dx dy]] (if (= value (get-in coll [(+ dx x) (+ dy y)]))
                              (inc acc)
                              acc)) 0 eight-neigbour))

;(defn simulate [input]
;  (let [occupied (count-neighbors input 0 0 )]))

(def a (for [j (range (count input))
      i (range (count (first input)))]
  (count-neighbors input i j \L))
)

;(println "Part 1 result: " 1)
;(println "Part 2 result: " 1)

;input



;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
