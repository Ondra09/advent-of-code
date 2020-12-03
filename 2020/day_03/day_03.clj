#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/2

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  line)

(defn modify-input [col]
  (vec col))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (map #(modify-input %))))

(def slope [3 1])
(def line-length (count (nth input 0)))

(defn get-x [slope]
  (nth slope 0))
(defn get-y [slope]
  (nth slope 1))


(defn check-slope [input slope]
  (for [[i y] (map-indexed vector(range (get-y slope) (count input) (get-y slope)))
        :let [x (mod (* (inc i) (get-x slope)) line-length)
              val (nth (nth input y) x)]]
    (if (= val \.) 0 1)))

(println "Part 1 result: " (apply + (check-slope input slope)))

(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(defn count-lines [input slopes]
  (map #(check-slope input %) slopes))

(def result-2 (->> slopes
                   (#(count-lines input %))
                   (map #(apply + %))
                   (apply *)))

(println "Part 2 result: " result-2)
