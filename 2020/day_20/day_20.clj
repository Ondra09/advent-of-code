#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/11

(defn split-input [s]
  (str/split s #"\n\n"))

(defn extract-column [input fun]
  (apply str (map fun input)))

(def val-to-int {\# 1 \. 0})

(defn code->int [lane]
  (reduce #(+ (* 2 %1) (val-to-int %2)) 0 lane))

(defn reverse-string [s]
  (apply str (reverse s)))

;; unrotated rectangle looks like this
;; All sides are coded as numbers to simplify comparision
;;    a
;;  d   b
;;    c
;;
;; for rotated image we need to inverse number however like this
;;
;;     di
;;   c    a
;;     bi

(defn rotate-right [[a b c d]]
  [(reverse-string d) a (reverse-string b) c])

;;   ai
;; b     d
;;   ci
(defn flip-right [[a b c d]]
  [(reverse-string a) d (reverse-string c) b])

;; Flip down is not needet, it is flip-right + rotations
;;    c
;; id    ib
;;    a

(defn all-rotations [[a b c d]]
  (let [id [a b c d]
        r1 (rotate-right id)
        r2 (rotate-right r1)
        r3 (rotate-right r2)]
    [id r1 r2 r3]
    ))

(defn all-transformations [quad]
  (let [rotations (all-rotations quad)
        rotations-flip (all-rotations (flip-right quad))]
    (into [] (concat [] rotations rotations-flip))
    ))

(defn extract-interesting-parts [input]
  (let [a (first input)
        b (extract-column input last)
        c (last input)
        d (extract-column input first)
        transformations (all-transformations [a b c d])]
    (map (fn [quad] (map code->int quad)) transformations)))

(defn parse-input [line]
  (let [lines (str/split line #"\n")
        [_ tile-code] (re-matches #"Tile (\d+):" (first lines))]
    [tile-code {:transformations (extract-interesting-parts (rest lines))}]
    ))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                (into {})
                ))
;;   a
;; d   b
;;   c
(defn match-neighbor [[a0 b0 c0 d0] [a1 b1 c1 d1]]
  (let [res []
        res (if (= a0 c1) (conj res :a) res)
        res (if (= b0 d1) (conj res :b) res)
        res (if (= c0 a1) (conj res :c) res)
        res (if (= d0 b1) (conj res :d) res)]
    res))

;; (defn find-all-neighbours [all-rotations all-rotations-b]
;;   (for [x all-rotations]
;;     (for [y all-rotations-b]
;;       (let [match (match-neighbor x y)]
;;       (if (not-empty match) [x y] [])
;;       ))))

;; returns vector of maps nth map is from n-rotation to b-rotation
;; [{} {} {} {} {} {} {} {}] no available
;; [{1 [:b :d], 6 [:a :c] } {} {} {} {} {} {} {}]
;; First from left match to first and sixth form right on indexes :b :d and :a :c
(defn find-all-neighbours [all-rotations all-rotations-b]
  (reduce
   (fn [acc rot-a]
     (conj acc  (into {} (second (reduce
                        (fn [[count acc] rot-b]
                          (let [nbrs (match-neighbor rot-a rot-b)]
                            (if (empty? nbrs)
                              [(inc count) acc]
                              [(inc count) (conj acc [count nbrs])] )))
                        [0 []]
                        all-rotations-b)))))
   []
   all-rotations))

(defn compute-neighborhood [world]
  (reduce
   (fn [acc [name-1 neigh-1]]
     (assoc acc name-1
            (reduce (fn [acc [name-2 neigh-2]]
                      (let [nbrs (find-all-neighbours (neigh-1 :transformations) (neigh-2 :transformations))]
                        (if (or (every? empty? nbrs) (= name-1 name-2)) acc
                            (assoc acc name-2 nbrs))
                        ))
                    {}
                    world)))
   {} world))


(def corners
  (map (fn [m] (read-string (first m))) (filter (fn [[name rest]]
                                                  (= 2 (count rest)))
                                                (compute-neighborhood input))))


(println "Part 1 result: " (apply * corners))
(println "Part 2 result: " )
