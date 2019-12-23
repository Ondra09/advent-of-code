(ns day_13.day13
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(use 'clojure.test)

(load-file "../util/intcode-computer.clj")
(clojure.core/alias 'ic 'util.intcode_computer)

(defn split-input [s]
  (str/split s #","))

(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(read-string %))
                (ic/enlarge-buffer 100000)
                (vec)))
(count input)
(first input)

;; part 1
(println "Part 1: " (reduce
                     (fn [sum [x y code]]
                       (if (= code 2) (inc sum) sum))
                     0
                     (partition 3 (ic/out-params (ic/chomp-input input 0 0 [])))))

;; part 2
(def free-play-input (assoc input 0 2))

;; 0 is an empty tile.
;; 1 is a wall tile. Walls are indestructible barriers.
;; 2 is a block tile.
;; 3 is a horizontal paddle tile.
;; 4 is a ball tile. The ball moves diagonally and bounces off objects.

(defn partition-output [machine]
  (partition 3 (ic/out-params machine)))

(defn tile-name->code [tile]
  (cond
    (= tile :empty) 0
    (= tile :wall) 1
    (= tile :block) 2
    (= tile :paddle) 3
    (= tile :ball) 4))

(defn is-score? [[x y]]
  (and (= x -1) (= y 0)))

(defn get-score [lst]
  (some #(if (is-score? %) (nth % 2)) lst))

(def output (partition-output (ic/chomp-input free-play-input 0 0 [])))

(defn get-position-for-name [name lst]
  (let [code (tile-name->code name)]
    (some (fn [triplet]
            (let [[x y in-code] triplet]
              (if (= code in-code)
                [x y]
                nil))) lst)))

(defn joystick->move-code [mode]
  (cond
    (= mode :left) -1
    (= mode :right) 1
    (= mode :neutral) 0))

(defn move-joystick [[ball-x ball-y] [paddle-x paddle-y]]
  (cond
    (> ball-x paddle-x) :right
    (< ball-x paddle-x) :left
    :else :neutral))

(defn obtain-joystick-action [output]
    (let [ action ((comp joystick->move-code move-joystick)
                   (get-position-for-name :ball output)
                   (get-position-for-name :paddle output))]
      action))

;; remove
(obtain-joystick-action '((1 1 3)(5 0 4)))
(partition-output  (ic/chomp-input input 0 0 []))
;; remove

(defn update-board [board diffs]
  (reduce (fn[coll [x y code]]
            (conj (remove (fn[[cx cy ccode]] (and (= cx x) (= cy y)) ) coll) `(~x ~y ~code)))
          board
          diffs))


(defn play-game [input]
  (loop [out (ic/chomp-input input 0 0 [])
         board '()
         n 0]
    (let [new-board (update-board board (partition-output out))]
          (if (or (= n 50000) (not= :block (ic/status out)))
            [(ic/status out) (get-score new-board)]
            (recur (apply ic/chomp-input (ic/assoc-output (ic/out-params->in-params out) (conj [] (obtain-joystick-action new-board)))) new-board  (inc n))
            ))))


(println "part 2: " (second (play-game free-play-input)))
