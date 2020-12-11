#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/11

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  line)

(def input (->> (slurp "input-test")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))


;; ----> X
;; |
;; |
;; v
;; Y

(defn get-in-str [coll x y]
  (get (get coll y) x))

(def eight-neigbour [[-1 -1] [0 -1] [1 -1]
                     [-1 0] [1 0]
                     [-1 1] [0 1] [1 1]])

(defn count-neighbors [coll x y value]
  (reduce (fn [acc [dx dy]] (if (= value (get-in-str coll (+ dx x) (+ dy y)))
                              (do (println "dx "dx " " dy " " acc " val " value " getin " (get-in-str coll (+ dx x) (+ dy y))) (inc acc))
                              (do (println "dx "dx " " dy " " acc " val " value " getin " (get-in-str coll (+ dx x) (+ dy y))) 0))) 0 eight-neigbour))

(println "Part 1 result: " 1)
(println "Part 2 result: " 1)
input

(println "> " (get-in-str input 2 2))

(println (count-neighbors input 1 1 \L))

input



;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
