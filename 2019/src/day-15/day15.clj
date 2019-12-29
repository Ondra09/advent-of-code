(ns day_15.day15
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

(defn intcodemove->coords [move]
  (cond
    (= move 1) [0 1]
    (= move 2) [0 -1]
    (= move 3) [-1 0]
    (= move 4) [1 0]))

(defn add-two-positions [a b]
  (vec (map + a b)))

(defn droid-status->name [status]
  (cond
    (= status 0) :wall
    (= status 1) :moved
    (= status 2) :moved-target))

(defn pick-random-move []
  (inc (rand-int 4)))

;; part 1
(defn chomp-maze [maze-input]
  (loop [input maze-input
         sp 0
         base 0
         n 0
         current [0 0]
         visited #{}]
    (let [move (pick-random-move)
          out (ic/chomp-input input sp base (conj [] move))
          response (first (ic/out-params out))
          new-coords (if (not= response 0)
                       (add-two-positions current (intcodemove->coords move))
                       current)
          ;; aaa (println new-coords)
          [new-input new-sp new-ip new-data] (ic/out-params->in-params out)]
      (if (or (> n 100000) (= response 2) (not= :block (ic/status out)))
        [response new-coords visited]
        (recur new-input new-sp new-ip (inc n) new-coords (conj visited new-coords))))))

(def visited-maze
  (loop [[response final visited] (chomp-maze input)]
    (if (= response 2)
      [response final visited]
      (recur (chomp-maze input)))))

(def result-random-path (nth visited-maze 2))
(def target-coord (second visited-maze))
(count result-random-path)
(def result-distace-list (reduce
                          #(assoc %1 %2 99999)
                          {}
                          (conj result-random-path target-coord)))

(defn get-neighbourhood [maze [x y]]
  (select-keys maze  (list [(inc x) y]
                           [(dec x) y]
                           [x (inc y)]
                           [x (dec y)])))


(get-neighbourhood result-distace-list [0 0])

(defn update-list [node plist distance-coll]
  (let [neighbourhood (get-neighbourhood distance-coll node)
        node-dst (inc (get distance-coll node))]
    (reduce
     (fn [[coll ff] [v dst]]
       (if (> dst node-dst)
         [(assoc coll v node-dst) (conj ff v)]
         [coll ff]))
     [distance-coll plist]
     neighbourhood)))

(defn sum-path [start distance-list]
  (loop [process-list (list start)
         distance-list (assoc distance-list start 0)
         n 0]
    (if (or (empty? process-list) (> n 100000))
      distance-list
      (let [node (first process-list)
            plist (rest process-list)
            [dlist ff] (update-list node plist distance-list)]
        (recur ff dlist (inc n))))))

(println "Part 1: " (get (sum-path [0 0] result-distace-list) target-coord))
;; target coord -16 14
;; part 2

(defn scan-neighbourhood [input sp base & rest]
  (for [i (range 1 5)
        :let [out (ic/chomp-input input sp base (conj [] i))]]
    [(first (ic/out-params out)) (ic/out-params->in-params out)]))

(defn get-avail-moves-coll [coll]
  "takes frist applies fun and joins with rest of original collection"
  (keep-indexed (fn [idx [info rest]] (if (not= 0 info) [(inc idx) rest] nil)) coll))

(defn get-viable-locs-coll [cur-loc coll]
  (map
   (fn [[intcode machine]]
     [(add-two-positions cur-loc (intcodemove->coords intcode)) machine])
   coll))

(defn add-node-to-result [results coll]
  ""
  (reduce
   (fn [results [coord _]]
     (conj results coord)
     )
   results
   coll))

(defn insert-to-process-list [process-list discovered new-positions]
  (reduce (fn [coll [loc rest]]
            ;; (println loc)
            ;; (println discovered)
            (if (contains? discovered loc)
              coll
              (conj coll [loc rest])))
          process-list
          new-positions))

(defn union-maze [input]
  (loop [process-list  `([[0 0] [~input 0 0 '()]])
         n 0
         discovered-list #{[0 0]}
         oxygen-computer '()
         ]
    (if (or (empty? process-list) (> n 10000))
      [n discovered-list oxygen-computer]
      (let [node (first process-list)
            coord (first node)
            neighbourhood-info (apply scan-neighbourhood (second node))
            avail-moves-intcode (get-avail-moves-coll neighbourhood-info)
            new-poss (get-viable-locs-coll coord avail-moves-intcode)
            new-process-list (insert-to-process-list (rest process-list) discovered-list new-poss)
            discovered-list (add-node-to-result discovered-list new-poss)
            oxygen-computer (if (= coord [-16 14]) node oxygen-computer)
            ]
         (recur new-process-list (inc n) discovered-list oxygen-computer)))))

(def result2 (union-maze input))

(def oxygen-computer (nth result2 1))
;; we have oxygen computer, now use this as an input to modified algorigthm

(def result-distace-list-2 (reduce
                          #(assoc %1 %2 99999)
                          {}
                          (nth result2 1)))

(println "Part 2: " (second (apply max-key val (sum-path [-16 14] result-distace-list-2))))
