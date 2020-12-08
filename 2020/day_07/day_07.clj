#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/7

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ name items] (re-matches #"([ \w]+) bags contain (.*)" line)
        children (re-seq #"(\d) ([ \w]+) bag" items)]
    {name children}))

(def input (->> (slurp "input-test")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (into {})
                ))

(defn get-parents [col name]
  (->> col
       (filter (fn [[k v]] (str/includes? v name)))
       (map (fn [lst] (first lst)))))

(defn find-all-parents [nodes node-name]
  (loop [stack `(~node-name)
         parent-names #{}]
    (if (empty? stack)
      parent-names
      (let [parents (get-parents nodes (first stack))
            new-stack (into (rest stack) parents)
            new-parent-names (into parent-names parents)
            ]
        (recur new-stack new-parent-names)))))

(defn find-all-bags [nodes node-name]
  (loop [stack `(~node-name)
         sum 0]))
input
;(println "Part 1 result: " (count (find-all-parents input "shiny gold")))
;(println "Part 2 result: " 1)
;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
; (filter #(is-child? % "shiny gold") input)

; ((hash-map "abc" "aaa") "abc")
; (find-all-parents input "shiny gold")

;input
;(find-all-parents input "shiny gold")
