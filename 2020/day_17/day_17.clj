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


(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))

(defn input->map [input]
  (second (reduce (fn [[x gmap] lane]
                    [(inc x) (second (reduce (fn [[y acc] c] [(inc y) (assoc acc [x y 0 0] c)]) [0 gmap] lane))])
                  [0 {}] input)))

;; ----> Y
;; |
;; |
;; v
;; X

(def world (input->map input))

(def twenty-six-neighbourhood (for [x (range  3)
                                    y (range 3)
                                    z (range 3)
                                    :when (not= x y z 1)]
                                  [(dec x) (dec y) (dec z) 0]))

(def fourd-neighoburhood (for [x (range 3)
                               y (range 3)
                               z (range 3)
                               w (range 3)
                               :when (not= x y z w 1)]
                           [(dec x) (dec y) (dec z) (dec w)]))

(defn add-coords [[x0 y0 z0 w0] [x1 y1 z1 w1]]
  [(+ x0 x1) (+ y0 y1) (+ z0 z1) (+ w0 w1)])

(defn sum-occupied [input]
  (reduce #(if (= \# %2) (inc %1) %1) 0 input))

(defn stay-active? [cube neighbourhood]
  (and (= cube \#)
            (or (= neighbourhood 2)
                (= neighbourhood 3))))

(defn should-activate? [cube neighbourhood]
  (and (= cube \.)
       (= neighbourhood 3)))

(defn get-neighbourhood-coords [coord neigbhourhood]
  (mapv #(add-coords coord %) neigbhourhood))

(defn get-default [map key default]
  (if (= nil (map key)) default (map key)))

(defn push-to-neighbours [world neighbourhood]
  "simulates step and push values to all neighbouring cells"
  (reduce (fn [accum [coord c]]
            (if (= \# c)(let [neigh (get-neighbourhood-coords coord neighbourhood)]
                          (reduce (fn [accum coord]
                                    (assoc accum coord (inc (get-default accum coord 0))))  accum neigh))
                accum))
          {} world))

(defn simulate-step [neighbourhood world]
  (let [sum-map (push-to-neighbours world neighbourhood)]
    (reduce
     (fn [acc [coord val]]
       (let [c (get-default world coord \.)]
         (cond (stay-active? c val) (assoc acc coord \#)
               (should-activate? c val) (assoc acc coord \#)
               :else acc
               )))
     {} sum-map)))

(println "Part 1 result: " (count  (last (take 7 (iterate  (partial simulate-step twenty-six-neighbourhood) world)))))
(println "Part 2 result: " (count  (last (take 7 (iterate  (partial simulate-step fourd-neighoburhood) world)))))



;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
