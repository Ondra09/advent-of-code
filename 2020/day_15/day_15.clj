#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (str/split line #","))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                (flatten)
                (mapv read-string)
                ))

(defn compute-part-1 [input target]
  (let [[startmap idx] (reduce (fn [[res idx] val] [(assoc res val idx) (inc idx)]) [{} 1] input)]
    (loop [recmap startmap
           currval 0
           turn idx]
      (if (= turn target)
        [recmap currval turn]
        (recur (assoc recmap currval turn)
               (if (nil? (recmap currval))
                 0
                 (- turn (recmap currval)))
               (inc turn)))
      )))

(println "Part 1 result: " (second (compute-part-1 (flatten input) 2020)))
(println "Part 2 result: " (second (compute-part-1 (flatten input) 30000000)))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
