(ns day_10.day10
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(use 'clojure.test)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)

(defn parse-input [input]
  (-> input
      (str/trim)
      (str/split-lines)))

(def input (->> (slurp "input.txt")
                (parse-input)))


(defn get-x-y [input x y]
  (nth (nth input y) x))

(def test-input-raw ".#..#
.....
#####
....#
...##")

(def test-input (parse-input test-input-raw))

(defn index->coords [width index]
  [(mod index width) (int (/ index width))])

(defn input->list-of-coords [input]
  (map-indexed (fn [idx c] [(index->coords (count (first input)) idx) c]) (str/join "" input)))

(def coords-test (input->list-of-coords test-input))

(defn round [num]
  (double (/ (int (* 10000000 num)) 10000000)))

(defn get-max-station [input]
  (let [coords (input->list-of-coords input)]
    (apply (partial max-key second)
           (remove nil? (for [y (range 0 (count input))
                              x (range 0 (count (first input)))]
                          (if-let [meteor (= (get-x-y input x y) \#)]
                            [[x y] (->> coords
                                 (map
                                  (fn [coord]
                                    (when (= (second coord) \#)
                                      (apply s2D/vec-normalize (s2D/vec2D x y (first (first coord)) (second (first coord))))
                                      )))
                                 (map (fn [dir]
                                        (if-let [[x y] dir]
                                          [(round x) (round y)])))
                                 (reduce
                                  (fn [uniqs coord]
                                    (if coord
                                      (conj uniqs coord)
                                      uniqs))
                                  (set []))
                                 (count)
                                 (dec) ;; hack for self
                                 )]))))))
(def test-input-2-raw ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(def test-input-2 (parse-input test-input-2-raw))
(deftest part1-tests
  (is (= 210 (second (get-max-station test-input-2))))
  (is (= [11 13] (first (get-max-station test-input-2)))))

(get-max-station input)
(def part-1 (get-max-station input))
;; 247
(println "Part 1: " (second part-1))
;;
(first part-1)

(def up-vector [0 -1])

;; use polar coordinates
;; [angle distance]
(def test-station [11 13])

(def station [20 21])

(defn vec2D->angle [vec2D] (apply s2D/vec-diff->angle
                                  (concat up-vector (apply s2D/vec-normalize vec2D))))

(nth
 (seq
(->> input
     (input->list-of-coords)
     (filter #(= (second %1) \#))
     (map #(apply s2D/vec2D (concat station (first %1))))
     (map (fn [vectr] [vectr
                       [(s2D/normalize-angle (vec2D->angle vectr))
                        (apply s2D/vec-length vectr)]]))

     ;; (map (fn [coords] [(first coords) ]))
     ;; (sort (fn [coords] (second coords)))
     ;; (sort-by (juxt second first second))
     (remove (fn [[[vx vy] polar]]
               (and (= vx 0) (= vy 0))))
     (reduce
      (fn [points-map [coords [angle distance]]]
        (let [lst (get points-map angle)]
          (assoc points-map angle (concat lst [[coords distance]]) ))
        )
      (sorted-map))

     ))
 (dec 200)
 );; and take value with shortest distance
;; test input 2
;; -3 -11
;; 11 13 = 8 2

;; input
;;-1 -2
(+ 20 -1) ;;19, 2
(+ 21 -2)
