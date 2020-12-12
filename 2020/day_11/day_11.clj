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

(def input (->> (slurp "input")
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

(defn count-neighbors [coll x y value neighbours it]
  (reduce (fn [{:keys [dirs sum]} [dx dy]]
            (let [tile (get-in coll [(+ (* it dx) x) (+ (* it dy) y)])]
              (if (or (= nil tile)
                      (= \L tile)
                      (= value tile))
                {:dirs (conj dirs [dx dy]) :sum (if (or (= tile \L) (= tile nil)) sum (inc sum))}
                {:dirs dirs :sum sum })))
          {:dirs [] :sum 0} neighbours))

(defn count-neighbors-one [coll x y value]
  (count-neighbors coll x y value eight-neigbour 1))

(defn count-line-neighbors [coll x y value]
  (loop [neighbours eight-neigbour
         sum-all 0
         it 1]
    (if (> (count neighbours) 0)
      (let [{:keys [dirs sum]} (count-neighbors coll x y value neighbours it)]
        (recur (filterv (complement (into #{} dirs)) neighbours) (+ sum sum-all) (inc it))
        )
      {:dirs [] :sum sum-all})))

(defn tile-changing-to [tile occupied-neigh-count occupied-max]
  (cond (and (= \# tile) (>= occupied-neigh-count occupied-max)) \L
        (and (= \L tile) (= occupied-neigh-count 0)) \#
        :else false
  ))

(defn find-changes [input neighbourhood-fun occupied-max]
  (for [j (range (count input))
        i (range (count (first input)))
        :let [myself (get-in input [j i])
              occupied-neigh-count ((neighbourhood-fun input j i \#) :sum)
              changing-to (tile-changing-to myself occupied-neigh-count occupied-max)]
        :when changing-to]
    {:x j :y i :val changing-to}))


(defn simulate [input neighbourhood-fun occupied-max]
  (loop [input input]
    (let [changes (find-changes input neighbourhood-fun occupied-max)]
      (if (> (count changes) 0)
        (recur (reduce (fn [coll {:keys [x y val]}] (assoc-in coll [x y] val)) input (find-changes input neighbourhood-fun occupied-max)))
        input
        ))))


(defn sum-occupied [input]
  ((frequencies (flatten input)) \#))

(def stable (simulate input count-neighbors-one 4))
(def stable-2 (simulate input count-line-neighbors 5))

(println "Part 1 result: " (sum-occupied stable))
(println "Part 2 result: " (sum-occupied stable-2))



;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
