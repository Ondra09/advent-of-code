#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)
;; https://adventofcode.com/2020/day/11

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  line)

(def input (->> (slurp "input-test")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (map #(vec %))
                (to-array-2d)))


(defn get-neigh [coll])

(println "Part 1 result: " 1)
(println "Part 2 result: " 1)
(println input)

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
