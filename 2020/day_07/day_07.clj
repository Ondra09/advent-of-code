#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/7

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ name items] (re-matches #"([ \w]+) bags contain (.*)" line)
        children (re-seq #"(\d+) ([ \w]+) bag" items)]
    {name (->> children
               (map (fn [[_ val child-name]] {child-name (read-string val)}))
               (into {})
               )}))

(def input (->> (slurp "input")
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
            new-parent-names (into parent-names parents)]
        (recur new-stack new-parent-names)))))

(defn sum-all-bags [nodes node-name]
    (let [children (nodes node-name)]
      (if (empty? children)
        0
        (apply + (map (fn [[k v]] (* v (inc (sum-all-bags nodes k)))) children)))))

(println "Part 1 result: " (count (find-all-parents input "shiny gold")))
(println "Part 2 result: " (sum-all-bags input "shiny gold"))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
